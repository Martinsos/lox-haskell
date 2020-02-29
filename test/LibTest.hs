module LibTest where

import Test.Tasty.Hspec

import Lib


spec_sayHello :: Spec
spec_sayHello = do
    it "Returns correct message" $ do
        sayHello `shouldBe` "Hello world!"