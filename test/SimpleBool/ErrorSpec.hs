{-# LANGUAGE OverloadedStrings #-}
module SimpleBool.ErrorSpec where

import Data.Monoid
import Test.Hspec

import Data.Info
import Data.Display
import SimpleBool.Error

spec :: Spec
spec = do
  describe "SimpleBool Error data-types" $ do
    describe "as an instance of Display type-class" $ do

      it "NoRuleApplies" $
        toDisplay NoRuleApplies `shouldBe` "[TERMINATE] NoRuleApplies"

      it "SomethingWrong" $
        toDisplay (SomethingWrong "maybe implement mistake") `shouldBe`
          "[ERROR] Something went wrong : maybe implement mistake"

      it "Wrong Binding" $ do
        let info = FileImput "test" 1 1
        toDisplay (WrongBinding info "x") `shouldBe`
          "[ERROR] Wrong kind of binding for variable : x\n" <>
          "file: test (line: 1, column: 1)"

      it "Out of Index by the Context" $ do
        let info = FileImput "test" 1 10
        toDisplay (OutOfContext info 1 1) `shouldBe`
          "[ERROR] Out of Index of the Context : (index: 1, context-depth: 1)\n" <>
          "file: test (line: 1, column: 10)"
