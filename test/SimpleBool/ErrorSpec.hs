{-# LANGUAGE OverloadedStrings #-}
module SimpleBool.ErrorSpec where

import Data.Monoid
import Test.Hspec

import Data.Info
import Data.Display
import SimpleBool.Type
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

      it "Not found named symbol" $ do
        let info = FileImput "test" 1 5
        toDisplay (UndefinedSymbol info "foo") `shouldBe`
          "[ERROR] Undefined Symbol : foo\n" <>
          "file: test (line: 1, column: 5)"

      it "Mismtach Types" $ do
        {-
          Example case: (x : Bool -> not x) 10
          function require type: Bool
          but applies value has: Number
        -}
        let info = FileImput "test" 1 10
        toDisplay (MismatchType info TyBool (TyArr TyBool TyBool)) `shouldBe`
          "[ERROR] Mismatch Type : Bool with Bool -> Bool\n" <>
          "file: test (line: 1, column: 10)"

      it "Cannot Type Unification" $ do
        {-
          Example case: if x then true else 0
          then type: Bool
          else type: Nat
          expression include 2 types (cannot type unification)
        -}
        let info = FileImput "test" 1 10
        toDisplay (CannotTypeUnify info TyBool (TyArr TyBool TyBool)) `shouldBe`
          "[ERROR] Cannot Type Unification : Bool with Bool -> Bool\n" <>
          "file: test (line: 1, column: 10)"
