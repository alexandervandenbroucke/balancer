import Test.Hspec
import Flow (
  Edge(MkEdge), reverseE,
  Vertex(),
  mkGraph,
  mkFlowGraph,
  capacity, saturated,
  edges,
  graph,
  shortestPath)
import Data.List (sort)

main :: IO ()
main = hspec $ do
  describe "mkFlowGraph" $ do
    it "makes flow graphs from a given graph" $
      let g = mkGraph [(0,[1]),(1,[])]
          fg = mkFlowGraph g (const 0) 0 1
      in sort (edges (graph fg))
         `shouldBe`
         [MkEdge 0 1, MkEdge 2 0]

  describe "shortestPath" $ do
    it "finds the shortest path between two nodes." $
      let g = mkGraph [(0,[1,2]),(1,[2]),(2,[3]),(3,[])]
          cap e | e          `elem` edges g = 1
                | reverseE e `elem` edges g = -1
                | otherwise                 = 0
          fg = mkFlowGraph g cap 0 3
      in shortestPath fg 0 3 `shouldBe` Just [MkEdge 0 2, MkEdge 2 3]

  describe "saturated" $ do
    it "finds a saturated edge in a flowgraph" $
      let g = mkGraph [(0,[1,2]),(1,[2]),(2,[3]),(3,[])]
          cap (MkEdge u v) | u == 1 && v == 2 = 5
                           | otherwise = 20
          fg = mkFlowGraph g cap 0 3
      in saturated fg `shouldBe` Just (20,[MkEdge 4 0, MkEdge 0 2, MkEdge 2 3])
          
