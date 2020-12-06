module UtilsSpec where

import Test.Hspec
import Utils

t = "1\n23\n\n123\n1"

spec :: Spec
spec = do
  describe "Utils" $ do
    it "groupLines handles simple example" $ do
        groupLines t `shouldBe` [["1", "23"],["123", "1"]]
    it "groupLines' handles simple example" $ do
        groupLines' [] (lines t) `shouldBe` [["1", "23"],["123", "1"]]