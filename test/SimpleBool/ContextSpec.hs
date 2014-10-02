module SimpleBool.ContextSpec where

import Control.Monad.Reader

import Test.Hspec

import Data.Info
import SimpleBool.Error
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

      describe "able to get binding variable (immutable)" $ do

        it "can get last registered binding" $ do
          let result = runEval $ do
                env' <- pushContext "x" NameBind
                local (const env') $ getBind Unknown 0
          getEvaledResult result `shouldBe` Right ("x", NameBind)

        it "can get first registered binding" $ do
          let result = runEval $ do
                env1 <- pushContext "x" NameBind
                env2 <- local (const env1) $ pushContext "y" NameBind
                local (const env2) $ getBind Unknown 1
          getEvaledResult result `shouldBe` Right ("x", NameBind)

        it "throw Error when wrong index of context" $ do
          let result = runEval $ getBind Unknown 0
          getEvaledResult result `shouldBe` Left (OutOfContext Unknown 0 0)

      describe "can get binding variable (mutable)" $ do

        it "can get last registered binding" $ do
          let result = runEval $ do
                putContext "x" NameBind
                getBind Unknown 0
          getEvaledResult result `shouldBe` Right ("x", NameBind)

        it "can get first registered binding" $ do
          let result = runEval $ do
                putContext "y" NameBind
                putContext "x" NameBind
                getBind Unknown 1
          getEvaledResult result `shouldBe` Right ("y", NameBind)
