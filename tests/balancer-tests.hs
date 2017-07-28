import Test.Hspec
import Balancer (
  Record(MkRecord),
  Transfer(MkTransfer),
  executeTransfer, executeTransfers,
  aggregateRecords,
  balance,
  balancedEpsilon)

main :: IO ()
main = hspec $ do
  describe "aggregateRecords" $ do
    it "aggregates records by payer" $
      let records = [MkRecord 3 200,
                    MkRecord 2 300,
                    MkRecord 2 400,
                    MkRecord 1 100,
                    MkRecord 3 200,
                    MkRecord 1 100]
      in aggregateRecords records `shouldBe` [MkRecord 1 200,
                                              MkRecord 2 700,
                                              MkRecord 3 400]
  describe "executeTransfer" $ do
    it "transfers funds from one account to another" $
      executeTransfer (MkTransfer 2 1 100) [MkRecord 1 100,MkRecord 2 0]
      `shouldBe`
      [MkRecord 1 0, MkRecord 2 100]

  describe "executeTransfers" $ do
    it "transfers funds from one account to another" $
      let records   = [MkRecord 1  100,MkRecord 2 0]
          transfers = [MkTransfer 2 1 100, MkTransfer 1 2 100]
      in executeTransfers transfers records `shouldBe` records

  describe "balance" $ do
    it "provides transfers that balance the records" $
      let records = [MkRecord 1 200,
                     MkRecord 2 700,
                     MkRecord 3 400]
      in executeTransfers (balance records) records
         `shouldSatisfy` balancedEpsilon 0.01
