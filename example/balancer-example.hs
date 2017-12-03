import System.Environment (getArgs)

import qualified Balancer as B
import qualified Greedy   as G
import Types (Expense(..),Transfer,prettyTransfers)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n] -> go B.balance (read n)
    ["flow",n]   -> go B.balance                  (read n)
    ["greedy",n] -> go (\es -> G.balance es 0.01) (read n)
    _            -> do
      putStrLn "usage: main [flow | greedy] n"

go :: ([Expense String] -> [Transfer String]) -> Int -> IO ()
go balance n = do
  let records = [MkExpense (show i) (100 * i2d i) | i <- [1..n]]
  prettyTransfers (balance records)

i2d :: Int -> Double
i2d = fromInteger . toInteger
