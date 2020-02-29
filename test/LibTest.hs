module LibTest where

import Test.Tasty.Hspec

import Lib


spec_scanTokens :: Spec
spec_scanTokens = do
    it "Breaks string by lines and words" $ do
        scanTokens "First line\nSecond line" `shouldBe` ["First", "line", "Second", "line"]
