module Interpreter.EnvironmentTest where

import Test.Tasty.Hspec

import Data.Maybe (fromJust)

import qualified Interpreter.Environment as E
import Interpreter.Value (Value(..))


spec_getVar :: Spec
spec_getVar = do
    describe "Correctly gets var when" $ do
        it "is in current env" $ do
            let env = E.empty `E.setVar` ("foo", NumberValue 42)
            env `E.getVar` "foo" `shouldBe` Just (NumberValue 42)
        it "is only in enclosing env" $ do
            let env = E.enclosed $ E.empty `E.setVar` ("foo", NumberValue 42)
            env `E.getVar` "foo" `shouldBe` Just (NumberValue 42)
        it "is in both current and in enclosing env" $ do
            let env = (E.enclosed $ E.empty `E.setVar` ("foo", NumberValue 1)) `E.setVar` ("foo", NumberValue 2)
            env `E.getVar` "foo" `shouldBe` Just (NumberValue 2)

spec_assignVar :: Spec
spec_assignVar = do
    it "Correctly updates var value when it already exists in current env" $ do
        let env = E.empty `E.setVar` ("foo", NumberValue 42)
        let (Just env') = env `E.assignVar` ("foo", NumberValue 314)
        env' `E.getVar` "foo" `shouldBe` Just (NumberValue 314)

    it "Returns nothing if var does not exist in current or enclosing env" $ do
        let env = E.enclosed (E.empty `E.setVar` ("bar", NumberValue 42)) `E.setVar` ("test", StringValue "hi")
        env `E.assignVar` ("foo", NumberValue 314) `shouldBe` Nothing

    describe "Correctly updates var value when" $ do
        it "already exists in current env" $ do
            let env = E.empty `E.setVar` ("foo", NumberValue 42)
            let (Just env') = env `E.assignVar` ("foo", NumberValue 314)
            env' `E.getVar` "foo" `shouldBe` Just (NumberValue 314)
        it "exists only in enclosing env" $ do
            let env = E.enclosed $ E.empty `E.setVar` ("foo", StringValue "old")
            let (Just env') = env `E.assignVar` ("foo", StringValue "new")
            (fromJust (E.getEnclosingEnv env') `E.getVar` "foo") `shouldBe` Just (StringValue "new")
        it "exists both in current and in enclosing env" $ do
            let env = (E.enclosed $ E.empty `E.setVar` ("foo", NumberValue 1)) `E.setVar` ("foo", NumberValue 2)
            let (Just env') = env `E.assignVar` ("foo", NumberValue 3)
            env' `E.getVar` "foo" `shouldBe` Just (NumberValue 3)
            (fromJust (E.getEnclosingEnv env') `E.getVar` "foo") `shouldBe` Just (NumberValue 1)

