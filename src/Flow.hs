{- |
Module      : Flow
Description : A generic flow-graph library.
Maintainer  : alexander.vandenbroucke@gmail.com

This module implements flow graphs, and the Edmonson-Karp algorithm to
find the maximal flow. Construct a graph from an adjacency-list
representation using 'mkGraph'. After finding the maximal flow, you
can find the vertices and edges using 'vertices' and 'edges'.

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Flow (
  -- * Example
  -- $example
  
  -- * Directed Graph
  -- $directed-graph
  Vertex(..),
  Edge(..), reverseE,
  Graph(..), mkGraph, vertices, edges,
  
  -- * Flow Graph
  -- $flow-graph
  Capacity, Flow,
  FlowGraph, mkFlowGraph, graph, flow, capacity, source, sink,
  totalFlow,
  
  -- * Finding the maximal flow
  -- $max-flow
  maximalFlowGraph,
  shortestPath,
  saturated,
  residual
)
where

import           Control.Monad (join)
import           Data.Bifunctor (second)
import           Data.Function (on)
import qualified Data.List as L (minimumBy,partition)
import           Data.Maybe (isJust,fromJust,fromMaybe)
import           Data.Ord (Down(Down))
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed as U
import           GHC.ST (ST)

{- $example
Consider the following example:

> [v1,v2,v3] = [MkVertex i | i <- [0..2]]
> g = mkGraph [(v1,[v2,v3]),(v2,[v3]),(v3,[])]
> cap edge | edge          `elem` edges g = 2
>          | reverseE edge `elem` edges g = -2
>          | otherwise                    = 0
> fg = mkFlowGraph g cap v1 v2

Then at the REPL type:

>>> totalFlow fg
0
>>> totalFlow (maximalFlowGraph fg)
4.0
>>> flow (maximalFlowGraph fg) (MkEdge v1 v2)
2.0
>>> flow (maximalFlowGraph fg) (MkEdge v1 v3)
2.0
>>> flow (maximalFlowGraph fg) (MkEdge v2 v3)
2.0

The total flow is obtained by summing the the flow going out of @v1@, or in to
@v3@: 2.0 + 2.0 = 4.0
-}

-------------------------------------------------------------------------------
-- Directed graph datastructure

-- $directed-graph
-- The representation used here is an adjacency list. This is achieved by
-- nesting two @Vector@s (an unboxed 'U.Vector' inside a boxed 'V.Vector').
--
-- To avoid having to define unboxing instances for 'Vertex', the unboxed
-- vector immediately contains the 'Int' ids of the vertex.


-- | A vertex, identified by an @Int@ identifier.
newtype Vertex
  = MkVertex
    {
      getId :: Int
    }
  deriving (Num,Eq,Ord,Show)

-- | An edge, a pair of two vertices.
data Edge
  = MkEdge
    {
      sourceV  :: Vertex,
      -- ^ The source vertex
      targetV :: Vertex
      -- ^ The target vertex
    }
  deriving (Eq,Ord,Show)

-- | A directed graph, in adjacency list representation.
--
--   Note: it is /not/ recommended to use the constructor @MkGraph@ directly.
--   Instead, use 'mkGraph'.
--
--   The datastructure also keeps a weight along the edge.
newtype Graph
  = MkGraph
    {
      unGraph :: V.Vector (U.Vector (Int,Double))
    }
    deriving Show

-- | The number of vertices in the 'Graph'
size :: Graph -> Int
size = V.length . unGraph

-- | Make a 'Graph' from an adjacency list.
--   The list contains pairs of vertices and lists of vertices.
--
--   For every pair, the list of vertices indicate the outgoing edges of the
--   vertex of the pair.
--   That is, for every pair, the resulting graph will have an edge from the
--   first component of the pair to every vertex in the second component.
--
--   Every vertex that occurs in a list should also occur as the first
--   component of a pair.
--
--   Moreover, for a list of length @n@, the vertices' ids should range over
--   [0..(n-1)].
mkGraph :: [(Vertex,[Vertex])] -> Graph
mkGraph adjList =
  let adjList' =
        [( getId v, U.fromList [(getId v,0) | v <- vs] ) | (v,vs) <- adjList]
      initVector = V.replicate (length adjList') U.empty
      g = V.accum (\_ vs -> vs) initVector adjList'
  in MkGraph g

-- | Get all the vertices in a Graph.
vertices :: Graph -> [Vertex]
vertices = map (MkVertex . fst) . V.toList . V.indexed . unGraph

-- | Get all the edges in a graph.
--
--   Note that there is a performance cost associated with this, as
--   Edges are not stored in this representation internally.
edges :: Graph -> [Edge]
edges = edges' . unGraph

-- | Get all the edges in a graph, working directly on the adjacency list
--   representation.
edges' :: V.Vector (U.Vector (Int,Double)) -> [Edge]
edges' vector =
  let ixs = V.toList (V.indexed vector)
      adjList2edges (v,adjs) = [pair2edge (v, i) | (i,_) <- U.toList adjs]
  in concatMap adjList2edges ixs

-- | Retain only edges that satisfy a condition.
--
--   The vertices will remain!
filterEdges :: (Edge -> Bool) -> Graph -> Graph
filterEdges p =
  let pNeighbour u (v,_) = p (pair2edge (u,v))
  in MkGraph . V.imap (\v -> U.filter (pNeighbour v)) . unGraph

-- | Add an Edge to the Graph.
--   The vertices will be added if they do not yet exist.
addEdge :: Edge -> Graph -> Graph
addEdge e (MkGraph g) =
  let u  = sourceV e
      v  = targetV e
      padLength = max (getId u) (getId v) + 1 - V.length g 
      padding = V.replicate padLength U.empty
      g' = V.modify (addEdge' u v 0) (g V.++ padding)
  in MkGraph g'

-- | Add an edge between to given nodes to the graph.
--   The source vertex must already exist in the graph!
--   Adding an edge with a non-existent source edge leads to undefined
--   behaviour.
addEdge' :: Vertex -> Vertex -> Double
         -> V.MVector s (U.Vector (Int,Double))
         -> ST s ()
addEdge' u v c mvector = do
  vs <- M.unsafeRead mvector (getId u)
  let vs' = case U.findIndex ((== getId v) . fst) vs of
        Just i  -> vs U.// [(i,(getId v,c))]
        Nothing -> U.cons (getId v,c) vs
  M.unsafeWrite mvector (getId u) vs'

-- | Add and Remove edges (and their capacity along it) simultanously.
--   When adding or removing an edge (u,v), the vertex u should already exist.
addAndRemoveEdges :: [(Double,Edge)] -> [Edge] -> Graph -> Graph
addAndRemoveEdges toAdd toRemove (MkGraph g) =
  let updates =
         [(getId u, addVertex v c) | (c,MkEdge u v) <- toAdd]
         ++
         [(getId u, removeVertex v) | MkEdge u v <- toRemove]
      addVertex v c vs =
        case U.findIndex ((getId v ==) . fst) vs of
          Just i  -> vs U.// [(i,(getId v,c))]
          Nothing -> U.cons (getId v,c) vs
      removeVertex v vs = U.filter ((getId v /=) . fst) vs
  in MkGraph (V.accum (\vs f -> f vs) g updates)

-- | Reverse an edge.
--   Note that:
--
--   prop> reverseE . reverseE == id
reverseE :: Edge -> Edge
reverseE (MkEdge x y) = MkEdge y x

-- | Deconstruct an edge into a pair of its identifiers.
edge2pair :: Edge -> (Int,Int)
edge2pair (MkEdge (MkVertex u) (MkVertex v)) = (u,v)

-- | Construct an edge from a pair of its identifiers.
pair2edge :: (Int,Int) -> Edge
pair2edge (u,v) = (MkEdge (MkVertex u) (MkVertex v))

-- | Get all outgoing edges of a 'Vertex' in a 'Graph'.
--
--   Note that there is a performance cost associated with this, as these
--   Edges are not stored in this representation internally.
outgoing :: Graph -> Vertex -> [Edge]
outgoing g u =
  let vs = unGraph g V.! getId u
  in map (\(v,_) -> pair2edge (getId u,v)) (U.toList vs)

-- | Set the weights on all edges in the graph.
--
--   The weights are determined by the correspondig value of the supplied
--   function.
setWeights :: (Edge -> Double) -> Graph -> Graph
setWeights weight (MkGraph g) = 
  let g' = V.imap (\i adj -> U.map (setAdjWeight i . fst) adj) g
      setAdjWeight u v = (v,weight (pair2edge (u,v)))
  in MkGraph g'

-- | Get the weight of an 'Edge', if the edge exists.
getWeight :: Edge -> Graph -> Maybe Double
getWeight (MkEdge u v) g = do
  adj   <- unGraph g V.!? getId u
  (_,c) <- U.find ((getId v ==) . fst) adj
  return c


-------------------------------------------------------------------------------
-- Flow Graph

-- $flow-graph
-- Flow graphs are directed graphs that additionally define a 'Flow' @f@ and
-- 'Capacity' @c@. Both are functions from Edges to Reals. Flow graphs have
-- a /source/ vertex @s@, where the flow originates and a /sink/ vertex @t@,
-- where the flow drains away.
--
-- Moreover, flow should satisfy the following properties:
--
-- /Capacity Constraint/: @f(u,v) < c(u,v)@ (for all vertices u,v)
--
-- /Skew-symmetry/: @f(u,v) = -f(v,u)@ (for all vertices u,v)
--
-- /Flow-preservation/: @sum_{vertex v} f(v,u) = 0@ (for all vertices u /= s,t)
--
-- The capacity should also satisfy Skew-symmetry.
-- 
-- Flow graphs can be created with 'mkFlowGraph'

type Flow     = Edge -> Double
type Capacity = Edge -> Double

-- | A flow graph.
--
--   Consists of a directed graph, flow, capacity and source and sink
--   vertices.
data FlowGraph =
  MkFlowGraph
  {
    graph    :: !Graph,
      -- ^ Directed graph of a 'FlowGraph'
    capacity :: Capacity,
      -- ^ Capacity function of a 'FlowGraph'
    source   :: !Vertex,
      -- ^ Source vertex of a 'FlowGraph'
    sink     :: !Vertex
      -- ^ Sink vertex of a 'FlowGraph'
  }


-- | Create a flow graph.
--
--   @mkFlowGraph g c s t@ creates a 'FlowGraph' with directed graph @g@,
--   capacity function @c@, source vertex @s@ and sink vertex @t@.
--   The flow is initialised to 0 on all edges.
mkFlowGraph :: Graph -> Capacity -> Vertex -> Vertex -> FlowGraph
mkFlowGraph graph' capacity' source' sink =
  let vs      = vertices graph'
      source  = MkVertex (size graph')
      graph   = addEdge (MkEdge source source') graph'
      maxCapacity = sum $ map capacity' $ outgoing graph' source'
      capacity e | sourceV e == source = 1 + maxCapacity
                 | otherwise           = capacity' e
      capacityGraph = setWeights capacity graph
  in MkFlowGraph capacityGraph capacity source sink

-- | Compute the total (current) flow in the 'FlowGraph'.
--
--   This is the flow through the source vertex (or sink vertex).
totalFlow :: FlowGraph -> Double
totalFlow fg = sum $ map (flow fg) $ outgoing (graph fg) (source fg)

-- | The flow on an edge of a flow graph.
flow :: FlowGraph -> Flow
flow fg e = capacity fg e - available fg e

-- | Available capacity of a 'FlowGraph'
--   This is the difference between capacity and flow
available :: FlowGraph -> Capacity
available fg e = fromMaybe 0 (getWeight e (graph fg))


-------------------------------------------------------------------------------
-- Maximal flow computation.

-- $max-flow
-- The functions in this section implement the Edmonson-Karp algorithm to
-- find the maximal flow graph, that is the flow graph with the largest total
-- flow of all flow graphs with the same directed graph, capacity, source and
-- sink.
--
-- Given a 'FlowGraph' @fg@, the following computes the maximal total flow:
--
-- > totalFlow (maximalFlowGraph fg)
--
-- The time complexity is O(V.E^2) where V is the number of vertices, E is the
-- number of edges.

-- | Find the maximal flow graph for a given flow graph.
--
--   By repeatedly searching for the shortest saturated path and taking the
--   residual flow graph.
maximalFlowGraph :: FlowGraph -> FlowGraph
maximalFlowGraph fg = case saturated fg of
  Nothing -> fg
  Just (capacity,path) -> maximalFlowGraph (residual capacity path fg)

-- | Find a shortest path from the source to the sink.
--
--   If such a path exists, the function returns the smallest capacity across
--   any edges along the path and the sequence of edges that make up the path,
--   otherwise it returns @Nothing@.
saturated :: FlowGraph -> Maybe (Double,[Edge])
saturated fg = do
  path           <- shortestPath fg (source fg) (sink fg)
  capacityOnPath <- minimumBy compare (map (available fg) path)
  return (capacityOnPath,path)

-- | Find the residual flow graph of a given flow graph.
--
--   For a given amount of flow and a path, the residual flow graph is
--   obtained by adding the given amount of flow to each edge along the path,
--   and substract the amount from the flow from the edge in the reverse
--   direction.
residual :: Double -> [Edge] -> FlowGraph -> FlowGraph
residual amount path fg =
    let pathR = map reverseE path
        (toAdd,toRemove) =
          let availablePath = [(available fg e - amount,e) | e <- path]
          in second (map snd) (L.partition ((> 0) . fst) availablePath)
        toAddR = [ (available fg e + amount,e) | e <- pathR]
        --
        newGraph = addAndRemoveEdges (toAdd ++ toAddR) toRemove (graph fg)
    in fg{graph = newGraph}

-- | Find the shortest path in a flow graph @fg@ from a given start vertex and
--   end vertex, only edges @e@ s.t. @flow fg e < cap fg e@ are considered
--
--   Uses BFS, hence O(E + V) amortized.
shortestPath :: FlowGraph -> Vertex -> Vertex -> Maybe [Edge]
shortestPath fg start end = 
  let g = graph fg
      reverseEdge0 =
        U.replicate (V.length (unGraph g)) (-1)
        U.// [(getId start, getId start)]
        -- -1 indicates unvisited
        -- the start node is already marked as visisted
      go :: S.Seq Vertex -> U.Vector Int -> U.Vector Int
      go queue reverseEdge = case S.viewl queue of
        S.EmptyL -> reverseEdge
        from S.:< remaining | from == end -> reverseEdge
        from S.:< remaining -> 
          let outV         = map fst (U.toList (unGraph g V.! getId from))
              unVisisted i = reverseEdge U.! i == -1
              outs         = filter unVisisted outV
              reverseEdge' = reverseEdge U.// [(i,getId from) | i <- outs]
              enqueue v queue' = queue' S.|> MkVertex v
          in go (foldr enqueue remaining outs) reverseEdge'
      getPath :: U.Vector Int -> Maybe [Edge]
      getPath reverseEdge =
        let getPath' v path = case reverseEdge U.! v of
              -1         -> Nothing
              u | u == v -> Just path
              u          -> getPath' u (pair2edge (u, v):path)
        in getPath' (getId end) []
    in getPath (go (S.singleton start) reverseEdge0)

-- Note: since all edges have length 1, we don't need a priority queue, a
-- simple FIFO queue will do.

-- | Find the minimum of a given list by a given ordering.
--
--   This function is a version of 'L.minimumBy' that also works on empty
--   lists. It returns 'Nothing' when given an empty list.
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
