{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Flow (
  Vertex(..),
  Edge(..), edge2pair,
  Graph(..), mkGraph, vertices, edges,
  FlowGraph, mkFlowGraph, graph, flow, capacity, source, sink,
  maximalFlowGraph,
  totalFlow,
  -- * Internal stuff
  shortestPath,
  saturated,
  residual
)
where

import           Control.Monad (join)
import           Data.Function (on)
import qualified Data.List as L (minimumBy)
import           Data.Maybe (isJust,fromJust,fromMaybe)
import           Data.Ord (Down(Down))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed as U
import           GHC.ST (ST)
import qualified Data.Sequence as S

newtype Graph
  = MkGraph
    {
      unGraph :: V.Vector (U.Vector Int)
    }
    deriving Show

newtype Vertex
  = MkVertex
    {
      getId :: Int
    }
  deriving (Num,Eq,Ord,Show)

data Edge
  = MkEdge
    {
      leftV  :: Vertex,
      rightV :: Vertex
    }
  deriving (Eq,Ord,Show)

size :: Graph -> Int
size = V.length . unGraph

mkGraph :: [(Vertex,[Vertex])] -> Graph
mkGraph adjList =
  let adjList' = map (\(v,vs) -> (getId v,U.fromList (map getId vs))) adjList
      initVector = V.replicate (length adjList') U.empty
      g = V.accum (\_ vs -> vs) initVector adjList'
  in MkGraph g

vertices :: Graph -> [Vertex]
vertices = map (MkVertex . fst) . V.toList . V.indexed . unGraph

edges :: Graph -> [Edge]
edges = edges' . unGraph

edges' :: V.Vector (U.Vector Int) -> [Edge]
edges' vector =
  let ixs = V.toList (V.indexed vector)
  in concatMap (\(v,vs) -> map (\i -> pair2edge (v,i)) (U.toList vs))  ixs
                                      
filterEdges :: (Edge -> Bool) -> Graph -> Graph
filterEdges p =
  let pNeighbour u v = p (pair2edge (u,v))
  in MkGraph . V.imap (\v -> U.filter (pNeighbour v)) . unGraph

-- | Add an Edge to the Graph.
--   The vertices will be added if they do not yet exist.
addEdge :: Edge -> Graph -> Graph
addEdge e (MkGraph g) =
  let u  = leftV e
      v  = rightV e
      padLength = max (getId u) (getId v) + 1 - V.length g 
      padding = V.replicate padLength U.empty
      g' = V.modify (addEdge' u v) (g V.++ padding)
  in MkGraph g'

addEdge' :: Vertex -> Vertex -> V.MVector s (U.Vector Int) -> ST s ()
addEdge' u v mvector = do
  vs <- M.unsafeRead mvector (getId u)
  let vs' = if U.elem (getId v) vs then vs else U.cons (getId v) vs
  M.unsafeWrite mvector (getId u) vs'

-- | Add and Remove vertices simultanously.
--   When adding or removing an edge (u,v), the vertex u should already exist.
addAndRemoveEdges :: [Edge] -> [Edge] -> Graph -> Graph
addAndRemoveEdges toAdd toRemove (MkGraph g) =
  let updates =
         [(getId u, addVertex v) | MkEdge u v <- toAdd]
         ++
         [(getId u, removeVertex v) | MkEdge u v <- toRemove]
      addVertex v vs =
        if U.elem (getId v) vs then
          vs
        else
          U.cons (getId v) vs
      removeVertex v vs = U.filter (/= getId v) vs
  in MkGraph (V.accum (\vs f -> f vs) g updates)

reverseE :: Edge -> Edge
reverseE (MkEdge x y) = MkEdge y x

edge2pair :: Edge -> (Int,Int)
edge2pair (MkEdge (MkVertex u) (MkVertex v)) = (u,v)


pair2edge :: (Int,Int) -> Edge
pair2edge (u,v) = (MkEdge (MkVertex u) (MkVertex v))

outgoing :: Graph -> Vertex -> [Edge]
outgoing g u =
  let vs = unGraph g V.! getId u
  in map (\v -> pair2edge (getId u,v)) (U.toList vs)

type Flow     = Edge -> Double
type Capacity = Edge -> Double

data FlowGraph =
  MkFlowGraph
  {
    graph    :: !Graph,
    flow     :: Flow,
    capacity :: Capacity,
    source   :: !Vertex,
    sink     :: !Vertex
  }

mkFlowGraph :: Graph -> Capacity -> Vertex -> Vertex -> FlowGraph
mkFlowGraph graph' capacity' source' sink =
  let vs      = vertices graph'
      source  = MkVertex (size graph')
      graph   = addEdge (MkEdge source source') graph'
      maxCapacity = sum $ map capacity' $ outgoing graph' source'
      capacity e | leftV e == source = 1 + maxCapacity
                 | otherwise         = capacity' e
      flow = const 0
  in MkFlowGraph graph flow capacity source sink

totalFlow :: FlowGraph -> Double
totalFlow fg = sum $ map (flow fg) $ outgoing (graph fg) (source fg)

maximalFlowGraph :: FlowGraph -> FlowGraph
maximalFlowGraph fg = case saturated fg of
  Nothing -> fg
  Just (capacity,path) -> maximalFlowGraph (residual capacity path fg)

saturated :: FlowGraph -> Maybe (Double,[Edge])
saturated fg = do
  let g = graph fg
  path           <- shortestPath g (source fg) (sink fg)
  capacityOnPath <- minimumBy compare (map (capacity fg) path)
  return (capacityOnPath,path)
  
residual :: Double -> [Edge] -> FlowGraph -> FlowGraph
residual cap path fg =
    let pathR = map reverseE path
        newCapacity e | e `elem` path  = capacity fg e - cap
                      | e `elem` pathR = capacity fg e + cap
                      | otherwise      = capacity fg e

        newFlow     e | e `elem` path = flow fg e + cap
                      | otherwise     = flow fg e
        toAdd = pathR
        toRemove = filter (\e -> newCapacity e <= 0) path
        newGraph = addAndRemoveEdges toAdd toRemove (graph fg)
        -- IDEA: make FlowGraphs monoidal, compose with a new graph made of the
        --       delta
    in fg{flow = newFlow, capacity = newCapacity, graph = newGraph}

shortestPath :: Graph -> Vertex -> Vertex -> Maybe [Edge]
shortestPath g start end = 
  let reverseEdge0 =
        U.replicate (V.length (unGraph g)) (-1)
        U.// [(getId start, getId start)]
        -- -1 indicates unvisited
        -- the start node is already marked as visisted
      go :: S.Seq Vertex -> U.Vector Int -> U.Vector Int
      go queue reverseEdge = case S.viewl queue of
        S.EmptyL -> reverseEdge
        vertex S.:< remaining | vertex == end -> reverseEdge
        vertex S.:< remaining -> 
          let outV         = U.toList (unGraph g V.! getId vertex)
              outs         = filter (\i -> reverseEdge U.! i == -1) outV
              reverseEdge' = reverseEdge U.// [(i,getId vertex) | i <- outs]
              enqueue v queue' = queue' S.|> MkVertex v
          in go (foldr enqueue remaining outs) reverseEdge'
      getPath :: U.Vector Int -> Maybe [Edge]
      getPath reverseEdge =
        let getPath' v path = case reverseEdge U.! v of
              -1         -> Nothing
              u | u == v -> Just path
              u -> getPath' u ((MkEdge (MkVertex u) (MkVertex v)):path)
        in getPath' (getId end) []
    in getPath (go (S.singleton start) reverseEdge0)

minimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBy cmp [] = Nothing
minimumBy cmp l  = Just (L.minimumBy cmp l)

-- -- testGraph :: FlowGraph
-- -- testGraph = mkFlowGraph graph capacity source sink where
-- --   graph = MkGraph [(0,[1,2]),(1,[2,3]),(2,[3])]
-- --   capacity (MkEdge x y) = case (x,y) of
-- --     (0,_)  -> 100.0
-- --     (1,2)  -> 1.0
-- --     (_,3)  -> 100.0
-- --     _      -> 0.0
-- --   source  = 0
-- --   sink = 3

-- -- testGraph2 :: FlowGraph
-- -- testGraph2 = mkFlowGraph graph capacity source sink where
-- --   graph = MkGraph [(0,[1,2]),(1,[3]),(2,[3]),(3,[4])]
-- --   capacity (MkEdge x y) = case (x,y) of
-- --     (0,1) -> 25.0
-- --     (0,2) -> 50.0
-- --     (1,3) -> 25.0
-- --     (2,3) -> 25.0
-- --     (3,4) -> 35.0
-- --     _     ->  0.0
-- --   source  = 0
-- --   sink = 4
