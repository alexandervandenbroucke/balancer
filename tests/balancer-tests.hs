import           Test.Hspec

import Types (
  Expense(MkExpense),
  Transfer(MkTransfer))

import Types (
  Expense(MkExpense),
  Transfer(MkTransfer),
  aggregate, balanced,
  Transferable(executeTransfer),
  executeTransfers)

import qualified Balancer as B
import qualified Greedy as G

main :: IO ()
main = hspec $ do
  describe "aggregate" $ do
    it "aggregates records by payer" $
      let records = [MkExpense 3 200,
                     MkExpense 2 300,
                     MkExpense 2 400,
                     MkExpense 1 100,
                     MkExpense 3 200,
                     MkExpense 1 100]
      in aggregate records `shouldBe` [MkExpense 1 200,
                                       MkExpense 2 700,
                                       MkExpense 3 400]
  describe "executeTransfer" $ do
    it "transfers funds from one account to another" $
      let t = MkTransfer 2 1 100
      in [executeTransfer t e | e <- [MkExpense 1 100,MkExpense 2 0]]
         `shouldBe`
         [MkExpense 1 0, MkExpense 2 100]

  describe "executeTransfers" $ do
    it "transfers funds from one account to another" $
      let records   = [MkExpense 1  100,MkExpense 2 0]
          transfers = [MkTransfer 2 1 100, MkTransfer 1 2 100]
      in map (executeTransfers transfers) records `shouldBe` records

  describe "Balancer.balance" $ do
    it "provides transfers that balance the records" $
      let records :: [Expense Integer]
          records = [MkExpense 1 200, MkExpense 2 700, MkExpense 3 400]
          transfers = B.balance records
      in map (executeTransfers transfers) records `shouldSatisfy` balanced 0.01

  describe "Greedy.balance" $ do
    it "provides transfers that balance the records" $
      let records :: [Expense Integer]
          records = [MkExpense 1 200, MkExpense 2 700, MkExpense 3 400]
          transfers = G.balance records 0.01
      in map (executeTransfers transfers) records `shouldSatisfy` balanced 0.01
