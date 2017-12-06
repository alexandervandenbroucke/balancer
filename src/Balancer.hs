module Balancer (
  Expense(..),
  Transfer(..),
  balance
)
where

import Data.List (sort)

import Flow (FlowGraph, Edge(MkEdge), Vertex(MkVertex),
             mkGraph, mkFlowGraph, maximalFlowGraph, graph,
             getId, edges, flow)
import Types (Expense(MkExpense,exp_payer,exp_amount),
              Transfer(MkTransfer),trans_amount)
import qualified Flow as F

-- | Provide a set of transfers such that the accounts balance, that is, every
--   payer pays the same amount. Note: every Expense must have a unique payer.
balance :: Ord a => [Expense a] -> [Transfer a]
balance expenses =
  let fg   = maximalFlowGraph (flowGraph expenses)
      n    = length expenses
      ins  = [inVertex  n i | i <- [1..n]]
      mids = [midVertex n i | i <- [1..n]]
      transferEdge (MkEdge u v) = (u `elem` ins) && (v `elem` mids)
      transferEdges = filter transferEdge (edges (graph fg))
      toTransfer e@(MkEdge u v) =
        let payerU = exp_payer (getExpense expenses (getId u))
            payerV = exp_payer (getExpense expenses (getId v))
        in MkTransfer payerU payerV (flow fg e)
  in [t | e <- transferEdges, let t = toTransfer e, trans_amount t > 0]


-- | Convert Int to Double
i2d :: Int -> Double
i2d = fromInteger . toInteger

-- | Construct a flow graph to such that its maximal flow, as computed by
--   the Edmonson-Karp algorithm balances the accounts.
--   This construction depends on the fact that the E-K algorithm uses the
--   shortest saturated path.
flowGraph :: [Expense a] -> FlowGraph
flowGraph expenses =
  let noExpenses = length expenses
      total   = sum (map exp_amount expenses)
      perNode = total / i2d noExpenses
      source = MkVertex 0
      sink   = MkVertex 1
      -- for every record there is an input, middle and output node
      ins  = [inVertex  noExpenses i | i <- [1..noExpenses]]
      mids = [midVertex noExpenses i | i <- [1..noExpenses]]
      outs = [outVertex noExpenses i | i <- [1..noExpenses]]
      equiv x y = (getId x - getId y) `mod` noExpenses == 0
      adjacencyList =
        -- source vertex is connected to all input vertices
        [(source,ins)] ++
        -- the input vertices are connected to their own output vertex, and all
        -- middle vertices except their own
        [(inV,let out = MkVertex (getId inV+2*noExpenses)
              in out:filter (not . equiv inV) mids)
        | inV <- ins] ++
        -- the middle vertices are connected to their output vertices
        -- this way flow between different accounts always has a longer path
        -- than flow from an account to itself.
        [(mid, [MkVertex (getId mid + noExpenses)]) | mid <- mids] ++
        -- the output vertices are connected to the sink vertex
        [(out,[sink])                         | out <- outs] ++
        [(sink,[])]
      capacity (MkEdge x y)
        | x == source && y /= sink   = perNode -- input flow capacity
        | y == sink   && x /= source =
            exp_amount (getExpense expenses (getId x))
            -- output flow capacity
        | x < y  =
            -- capacity from in to mid and out, and mid to out
            total + 1
        | otherwise = 0
  in mkFlowGraph (mkGraph adjacencyList) capacity source sink

-- | The input vertex corresponding to the i-th record
inVertex :: Int -> Int -> Vertex
inVertex _noExpenses i = MkVertex (1 + i)

-- | The input vertex corresponding to the i-th record
midVertex :: Int -> Int -> Vertex
midVertex noExpenses i = MkVertex (1 + noExpenses + i)

-- | The output vertex corresponding to the i-th record
outVertex :: Int -> Int -> Vertex
outVertex noExpenses i = MkVertex (1 + 2*noExpenses + i)

-- | Get the record corresponding to the input/middle/output vertex
getExpense :: [Expense a] -> Int -> Expense a
getExpense expenses i = expenses !! ((i - 2) `mod` length expenses)

-- | Debug pretty printing of edges
decode :: Int -> Edge -> String
decode noExpenses (MkEdge u v) =
  let decodeVertex (MkVertex 0) = "source"
      decodeVertex (MkVertex 1) = "sink"
      decodeVertex (MkVertex n)
        | n < 2+noExpenses   = "IN  " ++ show (n - 2)
        | n < 2+2*noExpenses = "MID " ++ show (n - 2 - noExpenses)
        | otherwise         = "OUT " ++ show (n - 2 - 2*noExpenses)
  in decodeVertex u ++ " -> " ++ decodeVertex v

-- | Debug pretty printing of a FlowGraph
debug :: Int -> FlowGraph -> Edge -> String
debug n flowG e@(MkEdge u v) =
  decode n e                ++ " " ++
  show (flow       flowG e) ++ "/" ++
  show (F.capacity flowG e)

-- | Debug printing
test :: [Expense a] -> IO ()
test expenses =
  let n  = length expenses
      fg = flowGraph expenses
      mfg = maximalFlowGraph fg
  in mapM_ (putStrLn . debug n mfg) (sort (edges (graph fg)))
