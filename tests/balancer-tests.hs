import Test.Hspec
import Balancer (
  Expense(MkExpense),
  Transfer(MkTransfer),
  executeTransfer, executeTransfers,
  aggregateExpenses,
  balance,
  balancedEpsilon)

main :: IO ()
main = hspec $ do
  describe "aggregateExpenses" $ do
    it "aggregates records by payer" $
      let records = [MkExpense 3 200,
                    MkExpense 2 300,
                    MkExpense 2 400,
                    MkExpense 1 100,
                    MkExpense 3 200,
                    MkExpense 1 100]
      in aggregateExpenses records `shouldBe` [MkExpense 1 200,
                                              MkExpense 2 700,
                                              MkExpense 3 400]
  describe "executeTransfer" $ do
    it "transfers funds from one account to another" $
      executeTransfer (MkTransfer 2 1 100) [MkExpense 1 100,MkExpense 2 0]
      `shouldBe`
      [MkExpense 1 0, MkExpense 2 100]

  describe "executeTransfers" $ do
    it "transfers funds from one account to another" $
      let records   = [MkExpense 1  100,MkExpense 2 0]
          transfers = [MkTransfer 2 1 100, MkTransfer 1 2 100]
      in executeTransfers transfers records `shouldBe` records

  describe "balance" $ do
    it "provides transfers that balance the records" $
      let records = [MkExpense 1 200, MkExpense 2 700, MkExpense 3 400]
      in executeTransfers (balance records) records
         `shouldSatisfy` balancedEpsilon 0.01
