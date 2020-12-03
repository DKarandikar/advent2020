module Day3Spec where

import Test.Hspec
import Day3

testInput = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"

spec :: Spec
spec = do
  describe "Day3" $ do
    it "doDay3 handles simple example" $ do
        doDay3 3 1 testInput `shouldBe` 7
    it "day3 part2 handles simple example" $ do
        doPart2 testInput `shouldBe` 336
