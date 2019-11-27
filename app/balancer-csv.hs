{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Monad ((>=>))
import           Data.Either (isLeft)
import           Data.List (partition,intercalate,sortOn)
import           Data.Maybe (fromMaybe,catMaybes)
import qualified Greedy as G
import           Prelude hiding (Read)
import           Text.ParserCombinators.ReadPrec (get,pfail)
import           Text.Read
import qualified Text.Read.CSV as CSV
import           Types
import           System.Environment (getArgs)

data Person
  = Bin
  | Cas
  | Didi
  | Ignace
  | Kamiel
  | Laucoon  
  | Lex
  | Senne
  | Shandro
  | Simon
  | Tom
  | VDF
  | Wouter
  deriving (Show,Read,Eq,Ord,Enum,Bounded)

defaultExpenses :: [Expense Person]
defaultExpenses = [MkExpense person 0 | person <- [minBound..maxBound]]

accounts :: [(Person,String)]
accounts = [
  (Didi,"AccountDidi"),
  (VDF, "AccountVDF"),
  (Lex, "AccountLex")]


newtype Euro = Euro Double deriving (Eq, Ord, Num,Fractional,Real,RealFrac)

instance Show Euro where
  show (Euro eur)
    | eur >= 0 = toEnum 0x20AC : show eur
    | otherwise = '-' : toEnum 0x20AC : show (abs eur)

instance Read Euro where
  readPrec = do
    let char c = get >>= \c' -> if c == c' then return () else pfail
        euroSign = char (toEnum 0x20AC)
    choice [
      euroSign >> fmap Euro readPrec,
      char '-' >> euroSign >> fmap (Euro . negate) readPrec
      ]

data Expenditure = Expenditure {
  by          :: Person,
  amount      :: Euro,
  date        :: String,
  description :: String
} deriving Show

main :: IO ()
main = do
  args <- getArgs
  let balance expenses = case args of
        ["greedy"] -> G.balance expenses 0.01
        _ -> balanceCentral Lex expenses
  str <- getContents
  case parseInput str of
    Nothing -> error "failed to parse expenditures"
    Just expenditures -> do
      let total = abs (sum (filter (<= 0) (map amount expenditures)))
          perPerson = total / 13
      putStrLn $ "Total expenditure: "
        ++ show (truncateMoney total)
        ++ ", per person: "
        ++ show (truncateMoney perPerson)

      let expenses = transferDeposits expenditures
      putStrLn $ "After deposit expenses: "
      tabulateExpenses perPerson expenses
      
      putStrLn $ "Balancing expenses ..."
      let eps = 0.01
          transfers = balance expenses
      
      putStr "Sanity Check: "
      let afterBalancing = executeTransfers transfers expenses
          sanityCheck e = abs (perPerson - Euro (exp_amount e)) < Euro eps
      if all sanityCheck afterBalancing then
        putStrLn "OK"
      else do
          putStrLn "NOT OK! Expenses:"
          tabulateExpenses perPerson afterBalancing
      putStrLn $
        "Balanced Check: "
        ++ if balanced eps afterBalancing then "OK" else "NOT OK!"

      let sortedTransfers = sortOn trans_from transfers
      putStrLn "Balancing transfers:"
      tabulateTransfers sortedTransfers

      putStrLn "Writing transfers to transfers.csv ..."
      outputTransfers "transfers.csv" sortedTransfers

parseInput :: String -> Maybe [Expenditure]
parseInput = CSV.readCSV >=> mapM parseExpenditure . drop 1

transferDeposits :: [Expenditure] -> [Expense Person]
transferDeposits expenditures =
  let (nonNeg,neg) = (partition ((>= 0) . amount) expenditures)
      expenses  = aggregate (map toExpense neg ++ defaultExpenses)
      transfers = map toTransfer nonNeg
  in executeTransfers transfers expenses

parseExpenditure [description,read -> amount,date,read -> by,comment] =
  Just (Expenditure{description,amount,date,by})
parseExpenditure _ = Nothing

toExpense :: Expenditure -> Expense Person
toExpense (Expenditure{by,amount = Euro euros}) = MkExpense by (abs euros)

toTransfer :: Expenditure -> Transfer Person
toTransfer (Expenditure{by,amount = Euro euros}) = MkTransfer by Lex euros 

tabulate :: [[String]] -> IO ()
tabulate table =
  let widths = foldr (zipWith max) [0..] (map (map length) table)
      printRow row = putStrLn (intercalate "  " (zipWith pad widths row))
      pad n str = str ++ replicate (n - length str) ' '
  in mapM_ printRow table

tabulateExpenses :: Show a => Euro -> [Expense a] -> IO ()
tabulateExpenses perPerson expenses =
  let inbalance e =
        let amt = Euro (exp_amount e) - perPerson
        in (if amt < 0 then "- " else "+ ") ++ truncateShow (abs amt)
  in tabulate [ [ " ",
                  show n ++ ".",
                  show (exp_payer e),
                  truncateShow (Euro $ exp_amount e),
                  "(" ++ inbalance e ++ ")"]
              | (n,e) <- zip [1..] expenses]


tabulateTransfers :: Show a => [Transfer a] -> IO ()
tabulateTransfers transfers =
  tabulate [ [" ",
              show n ++ ".",
              show (trans_from t),
              "->",
              show (trans_to t),
              truncateShow (Euro $ trans_amount t) ]
           | (n,t) <- zip [1..] transfers]

truncateMoney :: RealFrac a => a -> a
truncateMoney d = fromIntegral (round (d * 100)) / 100

truncateShow :: (RealFrac a, Show a) => a -> String
truncateShow = show . truncateMoney

-------------------------------------------------------------------------------
-- Central Balancing: balance through a central intermediary: all transfers are
-- to or from a central entity. Every participant has to perform at most
-- one transfer, except the central intermediary. In total there will be at
-- most @N - 1@ transfers, where @N@ is the number of participants, including
-- the intermediary. The balancing is exact.

-- | Balance a set of expenses through central balancing.
--
-- Assume there is at most one expense per participant in the list.
balanceCentral :: Eq a => a -> [Expense a] -> [Transfer a]
balanceCentral intermediary expenses =
  let totalExpense = sum (map exp_amount expenses)
      perParticipant = totalExpense / fromIntegral (length expenses)
      toTransfer MkExpense{exp_amount,exp_payer}
        | exp_payer == intermediary = Nothing
        | exp_amount > perParticipant =
            let trans_from = intermediary
                trans_to = exp_payer
                trans_amount = exp_amount - perParticipant
            in Just (MkTransfer{ trans_from, trans_to, trans_amount})
        | exp_amount == perParticipant = Nothing
        | otherwise =
            let trans_from = exp_payer
                trans_to = intermediary
                trans_amount = perParticipant - exp_amount
            in Just (MkTransfer{ trans_from, trans_to, trans_amount})
  in catMaybes (map toTransfer expenses)

-------------------------------------------------------------------------------
-- Output Transfers to CSV

prepareTransfers :: [Transfer Person] -> [[String]]
prepareTransfers transfers = titles : map prepareTransfer transfers where
  titles = ["From","To","Account number","Amount"]
  prepareTransfer MkTransfer{trans_from,trans_to,trans_amount} =
    [show trans_from,
     show trans_to,
     fromMaybe "" (lookup trans_to accounts),
     truncateShow (Euro trans_amount)]

outputTransfers :: FilePath -> [Transfer Person] -> IO ()
outputTransfers file = writeFile file . CSV.writeCSVstrict . prepareTransfers