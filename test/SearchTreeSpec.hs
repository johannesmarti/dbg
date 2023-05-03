module SearchTreeSpec (
   spec
) where

import Test.Hspec

import SearchTree

randomTree :: SearchTree Int
randomTree = n 1 [n 11 [n 111 [n 1111 [], n 1113 [n 4 []]], n 113 []],
                  n 13 [n 131 [], n 133 [n 1331 []], n 135 [], n 2 []]]
 where n l c = SearchTreeNode l c

spec :: Spec
spec = do
  describe "hotPathSearch in randomTree" $ do
    it "don't find an even on hot path" $
      hotPathSearch even 100 randomTree `shouldBe` []
    it "find 111" $
      hotPathSearch (== 111) 100 randomTree `shouldBe` [111]
    it "find 1 and 11 for predicate <100" $
      hotPathSearch (\n -> n < 100) 100 randomTree `shouldBe` [1,11]
    it "don't find 1111 if I only search to depth 2" $
      hotPathSearch (== 1111) 2 randomTree `shouldBe` []
  describe "hotPathSearch' in randomTree" $ do
    it "don't find an even on hot path" $
      hotPathSearch' even 100 randomTree `shouldBe` Nothing
    it "find 111" $
      hotPathSearch' (== 111) 100 randomTree `shouldBe` Just 111
    it "don't find 1111 if I only search to depth 2" $
      hotPathSearch' (== 1111) 2 randomTree `shouldBe` Nothing
  describe "depthFirstSearch in randomTree" $ do
    it "find 4 and 2 as even" $
      depthFirstSearch even 100 randomTree `shouldBe` [4,2]
    it "don't find 42" $
      depthFirstSearch (== 42) 100 randomTree `shouldBe` []
    it "don't find an even if I only search to depth 1" $
      depthFirstSearch even 1 randomTree `shouldBe` []
    it "find even 2 if I search to depth 2" $
      depthFirstSearch even 2 randomTree `shouldBe` [2]
  describe "depthFirstSearch' in randomTree" $ do
    it "find 4 as even" $
      depthFirstSearch' even 100 randomTree `shouldBe` Just 4
    it "don't find 42" $
      depthFirstSearch' (== 42) 100 randomTree `shouldBe` Nothing
    it "don't find an even if I only search to depth 1" $
      depthFirstSearch' even 1 randomTree `shouldBe` Nothing
    it "find even 2 if I search to depth 2" $
      depthFirstSearch' even 2 randomTree `shouldBe` Just 2
  describe "breadthFirstSearch in randomTree" $ do
    it "find 1" $
      breadthFirstSearch (== 1) 10 randomTree `shouldBe` [1]
    it "find 11" $
      breadthFirstSearch (== 11) 10 randomTree `shouldBe` [11]
    it "find 13" $
      breadthFirstSearch (== 13) 10 randomTree `shouldBe` [13]
    it "find 1113" $
      breadthFirstSearch (== 1113) 10 randomTree `shouldBe` [1113]
    it "find 4" $
      breadthFirstSearch (== 4) 10 randomTree `shouldBe` [4]
    it "find 2 and 4 as even" $
      breadthFirstSearch even 10 randomTree `shouldBe` [2,4]
    it "don't find 42" $
      breadthFirstSearch (== 42) 10 randomTree `shouldBe` []
    it "don't find an even if I only search to breadth 1" $
      breadthFirstSearch even 1 randomTree `shouldBe` []
    it "find even 2 if I search to breadth 2" $
      breadthFirstSearch even 2 randomTree `shouldBe` [2]
  describe "breadthFirstSearch' in randomTree" $ do
    it "find 1" $
      breadthFirstSearch' (== 1) 10 randomTree `shouldBe` Just 1
    it "find 11" $
      breadthFirstSearch' (== 11) 10 randomTree `shouldBe` Just 11
    it "find 13" $
      breadthFirstSearch' (== 13) 10 randomTree `shouldBe` Just 13
    it "find 1113" $
      breadthFirstSearch' (== 1113) 10 randomTree `shouldBe` Just 1113
    it "find 4" $
      breadthFirstSearch' (== 4) 10 randomTree `shouldBe` Just 4
    it "find 2 as even" $
      breadthFirstSearch' even 10 randomTree `shouldBe` Just 2
    it "don't find 42" $
      breadthFirstSearch' (== 42) 10 randomTree `shouldBe` Nothing
    it "don't find an even if I only search to breadth 1" $
      breadthFirstSearch' even 1 randomTree `shouldBe` Nothing
    it "find even 2 if I search to breadth 2" $
      breadthFirstSearch' even 2 randomTree `shouldBe` Just 2
  describe "dovetailingSearch in randomTree" $ do
    it "find 4 and 2 as even" $
      dovetailingSearch even 1000 randomTree `shouldBe` [4,2]
    it "don't find 42" $
      dovetailingSearch (== 42) 10 randomTree `shouldBe` []
    it "don't find an even if I only search 5 steps" $
      dovetailingSearch even 5 randomTree `shouldBe` []
  describe "dovetailingSearch' in randomTree" $ do
    it "find 4 as even" $
      dovetailingSearch' even 11 randomTree `shouldBe` Just 4
    it "don't find 42" $
      dovetailingSearch' (== 42) 10 randomTree `shouldBe` Nothing
    it "don't find an even if I only search 5 steps" $
      dovetailingSearch' even 5 randomTree `shouldBe` Nothing
