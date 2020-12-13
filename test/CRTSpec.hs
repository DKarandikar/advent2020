module CRTSpec where

import Test.Hspec ( describe, it, shouldBe, Spec )
import CRT

t = "1\n23\n\n123\n1"

spec :: Spec
spec = do
  describe "CRT" $ do
    it "gcd works" $ do
        CRT.gcd 10 6 `shouldBe` 2
    it "extended euclidean works" $ do
        CRT.extendedEu 240 46 `shouldBe` (-9, 47)
    it "crt works" $ do
        CRT.crt [(0, 17), (11, 13), (16, 19)] `shouldBe` 3417