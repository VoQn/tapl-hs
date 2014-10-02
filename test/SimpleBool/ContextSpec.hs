module SimpleBool.ContextSpec where

import Test.Hspec

import SimpleBool.Context

spec :: Spec
spec = do
  describe "SimpleBool Contexts" $ do
    describe "Control Context" $ do

      it "able to put new name-bind variable (immutable)" $ do
        let result = runEval $ pushContext "x" NameBind
        getEvaledContext result `shouldBe` []

      it "able to put new name-bind variable (mutable)" $ do
        let result = runEval $ putContext "x" NameBind
        getEvaledContext result `shouldBe` [("x", NameBind)]
