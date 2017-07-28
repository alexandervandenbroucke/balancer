module Balancer (
  Record(..),
  Transfer(..),
  prettyTransfers,
  balance,
  aggregateRecords
)
where

import Flow
import Data.List (sort,intercalate)
import Control.Monad (forM_)
import Data.Function (on)
import Data.List (groupBy, sortBy)

data Record a =
  MkRecord
  {
    rec_payer  :: a,
    rec_amount :: Double
  }
  deriving (Show,Eq,Ord)

data Transfer a =
  MkTransfer
  {
    trans_from   :: a,
    trans_to     :: a,
    trans_amount :: Double
  }
  deriving Show

aggregateRecords :: Ord a => [Record a] -> [Record a]
aggregateRecords records =
  let sorted = sortBy (compare `on` rec_payer) records
      grouped = groupBy ((==) `on` rec_payer) sorted
  in [MkRecord (rec_payer (head l)) (sum (map rec_amount l)) | l <- grouped]

prettyTransfers :: [Transfer String] -> IO ()
prettyTransfers transfers = do
  forM_ transfers $ \(MkTransfer from to amount) ->
    putStrLn (from ++ " -> " ++ to ++ ": " ++ show amount)


-- | Provide a set of transfers such that the accounts balance, that is, every
--   payer pays the same amount.
balance :: Ord a => [Record a] -> [Transfer a]
balance records =
  let fg   = maximalFlowGraph (flowGraph records)
      n    = length records
      ins  = [inVertex  n i | i <- [1..n]]
      mids = [midVertex n i | i <- [1..n]]
      transferEdge (MkEdge u v) = (u `elem` ins) && (v `elem` mids)
      transferEdges = filter transferEdge (edges (graph fg))
      toTransfer e@(MkEdge u v) =
        let payerU = rec_payer (getRecord records (getId u))
            payerV = rec_payer (getRecord records (getId v))
        in MkTransfer payerU payerV (flow fg e)
  in [t | e <- transferEdges, let t = toTransfer e, trans_amount t > 0]


-- | Convert Int to Double
i2d :: Int -> Double
i2d = fromInteger . toInteger

-- | Construct a flow graph to such that its maximal flow, as computed by
--   the Edmonson-Karp algorithm balances the accounts.
--   This construction depends on the fact that the E-K algorithm uses the
--   shortest saturated path.
flowGraph :: [Record a] -> FlowGraph
flowGraph records =
  let noRecords = length records
      total   = sum (map rec_amount records)
      perNode = total / i2d noRecords
      source = MkVertex 0
      sink   = MkVertex 1
      -- for every record there is an input, middle and output node
      ins  = [inVertex  noRecords i | i <- [1..noRecords]]
      mids = [midVertex noRecords i | i <- [1..noRecords]]
      outs = [outVertex noRecords i | i <- [1..noRecords]]
      equiv x y = (getId x - getId y) `mod` noRecords == 0
      adjacencyList =
        -- source vertex is connected to all input vertices
        [(source,ins)] ++
        -- the input vertices are connected to their own output vertex, and all
        -- middle vertices except their own
        [(inV,let out = MkVertex (getId inV+2*noRecords)
              in out:filter (not . equiv inV) mids)
        | inV <- ins] ++
        -- the middle vertices are connected to their output vertices
        -- this way flow between different accounts always has a longer path
        -- than flow from an account to itself.
        [(mid, [MkVertex (getId mid + noRecords)]) | mid <- mids] ++
        -- the output vertices are connected to the sink vertex
        [(out,[sink])                         | out <- outs]
      capacity (MkEdge x y)
        | x == source && y /= sink   = perNode -- input flow capacity
        | y == sink   && x /= source =
            rec_amount (getRecord records (getId x))
            -- output flow capacity
        | x < y  =
            -- capacity from in to mid and out, and mid to out
            total + 1
        | otherwise = 0
  in mkFlowGraph (MkGraph adjacencyList) capacity source sink

-- | The input vertex corresponding to the i-th record
inVertex :: Int -> Int -> Vertex
inVertex noRecords i = MkVertex (1 + i)

-- | The input vertex corresponding to the i-th record
midVertex :: Int -> Int -> Vertex
midVertex noRecords i = MkVertex (1 + noRecords + i)

-- | The output vertex corresponding to the i-th record
outVertex :: Int -> Int -> Vertex
outVertex noRecords i = MkVertex (1 + 2*noRecords + i)

-- | Get the record corresponding to the input/middle/output vertex
getRecord :: [Record a] -> Int -> Record a
getRecord records i = records !! ((i - 2) `mod` length records)

-- | Debug pretty printing of edges
decode :: Int -> Edge -> String
decode noRecords (MkEdge u v) =
  let decodeVertex (MkVertex 0) = "source"
      decodeVertex (MkVertex 1) = "sink"
      decodeVertex (MkVertex n)
        | n < 2+noRecords   = "IN  " ++ show (n - 2)
        | n < 2+2*noRecords = "MID " ++ show (n - 2 - noRecords)
        | otherwise         = "OUT " ++ show (n - 2 - 2*noRecords)
  in decodeVertex u ++ " -> " ++ decodeVertex v

-- | Debug pretty printing of a FlowGraph
debug :: Int -> FlowGraph -> Edge -> String
debug n flowG e@(MkEdge u v) =
  decode n e              ++ " " ++
  show (flow     flowG e) ++ "/" ++
  show (capacity flowG e)

-- | Debug printing
test :: [Record a] -> IO ()
test records =
  let n  = length records
      fg = flowGraph records
      mfg = maximalFlowGraph fg
  in mapM_ (putStrLn . debug n mfg) (sort (edges (graph fg)))
