import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "initBoard'" $ do
    it "returns a good board" $ do
      let board = initBoard
      length (grid board) `shouldBe` 4
      length (grid board!!0) `shouldBe` 4
      currentId board `shouldBe` 16

  describe "flatGrid" $ do
    it "returns a list of 16 elements" $
      length (flatGrid initBoard) `shouldBe` 16

  describe "emptyCells" $ do
    it "returns a list of 16 elements when all values is 0" $
      length (emptyCells initBoard) `shouldBe` 16

  describe "chooseRandomTile" $ do
    it "returns rights coords and value given random values" $ do
      let (x, y, v) = chooseRandomTile initBoard 0.2 0.1
      x `shouldBe` 0
      y `shouldBe` 3
      v `shouldBe` 4

  describe "reducer" $ do
    it "returns" $ do
      let state = State { board=initBoard, changed=False, won=False, lost=False }
      let action = Action { actionType=Init, direction=0, randomValue=0.2, randomPosition=0.8 }
      putStrLn (show (reducer state action))
