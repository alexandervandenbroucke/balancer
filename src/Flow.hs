{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flow (
  Vertex(..),
  Edge(..), edge2pair,
  Graph(..), vertices, edges,
  FlowGraph, mkFlowGraph, graph, flow, capacity, source, sink,
  maximalFlowGraph,
  totalFlow
)
where

import           Control.Monad (join)
import           Data.Function (on)
import qualified Data.List as L (minimumBy)
import           Data.Maybe (isJust,fromJust,fromMaybe)
import           Data.Ord (Down(Down))

newtype Graph
  = MkGraph
    {
      unGraph :: [(Vertex,[Vertex])]
    }

newtype Vertex = MkVertex { getId :: Int } deriving (Num,Eq,Ord,Show)
data Edge
  = MkEdge
    {
      leftV  :: Vertex,
      rightV :: Vertex
    }
  deriving (Eq,Ord,Show)

type Flow     = Edge -> Double
type Capacity = Edge -> Double

data FlowGraph =
  MkFlowGraph
  {
    graph    :: Graph,
    flow     :: Flow,
    capacity :: Capacity,
    source   :: Vertex,
    sink     :: Vertex
  }

mkFlowGraph :: Graph -> Capacity -> Vertex -> Vertex -> FlowGraph
mkFlowGraph graph' capacity' source' sink =
  let vs      = vertices graph'
      source  = minimum vs - 1
      graph   = MkGraph ((source,[source']):unGraph graph')
      maxCapacity = sum $ map capacity' $ outgoing graph' source'
      capacity (MkEdge (-1) _) = 1 + maxCapacity
      capacity e               = capacity' e
      flow = const 0
  in MkFlowGraph graph flow capacity source sink

vertices :: Graph -> [Vertex]
vertices = map fst . unGraph

edges :: Graph -> [Edge]
edges = concatMap (\(v,vs) -> map (MkEdge v) vs) . unGraph

edges' :: [(Vertex,[Vertex])] -> [Edge]
edges' = concatMap (\(v,vs) -> map (MkEdge v) vs)

filterEdges :: (Edge -> Bool) -> Graph -> Graph
filterEdges p =
  MkGraph .
  filter (not . null . snd) .
  map (\(v,vs) -> (v,filter (p . MkEdge v) vs)) .
  unGraph

addEdge :: Edge -> Graph -> Graph
addEdge e (MkGraph g) = MkGraph g' where
  u  = leftV e
  v  = rightV e
  vs = filter (/= v) $ fromMaybe [] $ lookup u g
  g' = (u,v:vs) : filter ((/= u) . fst) g 

reverseE :: Edge -> Edge
reverseE (MkEdge x y) = MkEdge y x

edge2pair :: Edge -> (Int,Int)
edge2pair (MkEdge (MkVertex u) (MkVertex v)) = (u,v)

outgoing :: Graph -> Vertex -> [Edge]
outgoing g v = edges' $ filter (\(u,_) -> u == v) $ unGraph g

totalFlow :: FlowGraph -> Double
totalFlow fg = sum $ map (flow fg) $ outgoing (graph fg) (source fg)

maximalFlowGraph :: FlowGraph -> FlowGraph
maximalFlowGraph fg = case saturated fg of
  Nothing -> fg
  Just e  -> maximalFlowGraph (residual e fg)

saturated :: FlowGraph -> Maybe Edge
saturated fg = do
  let g = graph fg
  p <- shortestPath g (source fg) (sink fg)
  minimumBy (compare `on` capacity fg) p
  
residual :: Edge -> FlowGraph -> FlowGraph
residual e fg = case shortestPath (graph fg) (source fg) (sink fg) of
  Nothing -> fg
  Just p  -> fromMaybe fg $ do
    e <- minimumBy (compare `on` capacity fg) p
    let pR = map reverseE p
    let c  = capacity fg e
        newCapacity e | e `elem` p  = capacity fg e - c
                      | e `elem` pR = capacity fg e + c
                      | otherwise   = capacity fg e
        newFlow     e | e `elem` p = flow fg e + c
                      | otherwise  = flow fg e
    let newGraph  = filterEdges (\e -> newCapacity e > 0) (graph fg)
        newGraph' = foldr addEdge newGraph pR
    -- IDEA: make FlowGraphs monoidal, compose with a new graph made of the
    --       delta
    return fg{flow = newFlow, capacity = newCapacity, graph = newGraph'}

shortestPath :: Graph -> Vertex -> Vertex -> Maybe [Edge]
shortestPath g start end = fmap (reverse . fst) (go [] [] 0 start) where
  go :: [Vertex] -> [Edge] -> Int -> Vertex -> Maybe ([Edge],Int) 
  go visited path len start
    | start == end = Just (path,len)
    | otherwise    =
        let outE     = outgoing g start
            outs     = filter (not . (`elem` visited) . rightV) outE
            visited' = visited ++ map rightV outs
            paths    =
              [ go visited' (v:path) (len+1) (rightV v) | v <- outs ]
            cmp = compare `on` Down . fmap (Down . snd)
        in join (minimumBy cmp paths)

minimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBy cmp [] = Nothing
minimumBy cmp l  = Just (L.minimumBy cmp l)

testGraph :: FlowGraph
testGraph = mkFlowGraph graph capacity source sink where
  graph = MkGraph [(0,[1,2]),(1,[2,3]),(2,[3])]
  capacity (MkEdge x y) = case (x,y) of
    (0,_)  -> 100.0
    (1,2)  -> 1.0
    (_,3)  -> 100.0
    _      -> 0.0
  source  = 0
  sink = 3

testGraph2 :: FlowGraph
testGraph2 = mkFlowGraph graph capacity source sink where
  graph = MkGraph [(0,[1,2]),(1,[3]),(2,[3]),(3,[4])]
  capacity (MkEdge x y) = case (x,y) of
    (0,1) -> 25.0
    (0,2) -> 50.0
    (1,3) -> 25.0
    (2,3) -> 25.0
    (3,4) -> 35.0
    _     ->  0.0
  source  = 0
  sink = 4
