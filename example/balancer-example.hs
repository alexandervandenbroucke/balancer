import System.Environment (getArgs)

import qualified Balancer as B
import qualified Greedy   as G
import Types (Expense(..),Transfer(MkTransfer),prettyTransfers)
import Control.DeepSeq (NFData(rnf),deepseq)

instance NFData a => NFData (Transfer a) where
  rnf (MkTransfer from to amount) =
    from `deepseq` to `deepseq` amount `deepseq` ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n] -> prettyTransfers $ go B.balance (read n)
    ["flow",n]    -> prettyTransfers $ go B.balance                  (read n)
    ["greedy",n]  -> prettyTransfers $ go (\es -> G.balance es 0.01) (read n)
    ["noprint",n] -> go (\es -> G.balance es 0.01) (read n) `deepseq` return ()
    _            -> do
      putStrLn "usage: main [flow | greedy] n"

go :: ([Expense String] -> [Transfer String]) -> Int -> [Transfer String]
go balance n = balance [MkExpense (show i) (100 * i2d i) | i <- [1..n]]

i2d :: Int -> Double
i2d = fromInteger . toInteger
