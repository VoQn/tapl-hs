module SimpleBool.ContextSpec where

import Control.Monad.Reader

import Test.Hspec hiding (context)

import Data.Info
import Data.Evaluator ((|->))
import SimpleBool.Error
import SimpleBool.Context

spec :: Spec
spec = describe "SimpleBool Contexts" $ do
  describe "Control Context" $ do

    it "able to put new name-bind variable (immutable)" $ do
      let result = runEvalTop $ pushContext "x" NameBind
      getEvaledContext result `shouldBe` []

    it "able to put new name-bind variable (mutable)" $ do
      let result = runEvalTop $ putContext "x" NameBind
      getEvaledContext result `shouldBe` [("x", NameBind)]

  describe "able to get binding variable (immutable)" $ do

    it "can get last registered binding" $ do
      let result = runEvalTop $
            ask
            |-> pushContext "x" NameBind
            |-> getBind Unknown 0
      getEvaledResult result `shouldBe` Right ("x", NameBind)

    it "can get first registered binding" $ do
      let result = runEvalTop $
            ask
            |-> pushContext "x" NameBind
            |-> pushContext "y" NameBind
            |-> getBind Unknown 1
      getEvaledResult result `shouldBe` Right ("x", NameBind)

    it "throw Error when wrong index of context" $ do
      let result = runEvalTop $ getBind Unknown 0
      getEvaledResult result `shouldBe` Left (OutOfContext Unknown 0 0)

  describe "can get binding variable (mutable)" $ do

    it "can get last registered binding" $ do
      let result = runEvalTop $ do
            putContext "x" NameBind
            getBind Unknown 0
      getEvaledResult result `shouldBe` Right ("x", NameBind)

    it "can get first registered binding" $ do
      let result = runEvalTop $ do
            putContext "y" NameBind
            putContext "x" NameBind
            getBind Unknown 1
      getEvaledResult result `shouldBe` Right ("y", NameBind)
