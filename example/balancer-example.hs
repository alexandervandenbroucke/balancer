import System.Environment (getArgs)

import Balancer(Expense(..),balance, prettyTransfers)

main :: IO ()
main = do
  [n] <- getArgs
  let records = [MkExpense (show i) (100* i2d i) | i <- [1..(read n)]]
  prettyTransfers (balance records)

i2d :: Int -> Double
i2d = fromInteger . toInteger
