{-# LANGUAGE OverloadedStrings #-}
module Chapter4.EvalSpec where

import Test.Hspec

import Chapter4.Display
import Chapter4.Syntax
import Chapter4.Eval

-- import Chapter4.SyntaxSpec hiding (spec)

spec :: Spec
spec = do

  describe "eval simple value" $ do

    it "can eval true" $
      eval TmTrue `shouldBe` Right TmTrue

    it "can eval false" $
      eval TmFalse `shouldBe` Right TmFalse

    it "can eval 0" $
      eval TmZero `shouldBe` Right TmZero

  describe "eval (succ <term>)" $ do

    it "can eval (succ 0)" $
      eval (TmSucc TmZero) `shouldBe` Right (TmSucc TmZero)

    it "can eval (succ (succ 0))" $
      eval (TmSucc $ TmSucc TmZero) `shouldBe`
      Right (TmSucc $ TmSucc TmZero)

    it "can eval (succ (pred 0))" $
      eval (TmSucc $ TmPred TmZero) `shouldBe`
      Right (TmSucc TmZero)

    it "cannot eval (succ true)" $
      eval (TmSucc TmTrue) `shouldBe`
      Left (TypeError TyNat "(succ x)" TmTrue)

  describe "eval (pred <term>)" $ do

    it "can eval (pred 0)" $
      eval (TmPred TmZero) `shouldBe` Right TmZero

    it "can eval (pred (succ 0))" $
      eval (TmPred $ TmSucc TmZero) `shouldBe` Right TmZero

    it "can eval (pred (pred 0))" $
      eval (TmPred $ TmPred TmZero) `shouldBe` Right TmZero

    it "cannot eval (pred true)" $
      eval (TmPred TmTrue) `shouldBe`
      Left (TypeError TyNat "(pred x)" TmTrue)

    it "cannot eval (pred (succ true))" $
      eval (TmPred $ TmSucc TmTrue) `shouldBe`
      Left (TypeError TyNat "(succ x)" TmTrue)

  describe "eval (zero? <term>)" $ do

    it "can eval (zero? 0)" $
      eval (TmIsZero TmZero) `shouldBe` Right TmTrue

    it "can eval (zero? (succ 0))" $
      eval (TmIsZero $ TmSucc TmZero) `shouldBe` Right TmFalse

    it "can eval (zero? (pred 0))" $
      eval (TmIsZero $ TmPred TmZero) `shouldBe` Right TmTrue

    it "cannot eval (zero? true)" $
      eval (TmIsZero TmTrue) `shouldBe`
      Left (TypeError TyNat "(zero? x)" TmTrue)

    it "cannot eval (zero? (succ true))" $
      eval (TmIsZero $ TmSucc TmTrue) `shouldBe`
      Left (TypeError TyNat "(succ x)" TmTrue)

  describe "eval (if <term> <term> <term>)" $ do

    it "can eval (if true true true)" $
      eval (TmIf TmTrue TmTrue TmTrue) `shouldBe`
      Right TmTrue

    it "can eval (if true true true)" $
      eval (TmIf TmFalse (TmSucc TmZero) TmZero) `shouldBe`
      Right TmZero

    it "can eval (if (zero? 0) false true)" $
      eval (TmIf (TmIsZero TmZero) TmFalse TmTrue) `shouldBe`
      Right TmFalse

    it "can eval (if (zero? (succ 0)) false true)" $
      eval (TmIf (TmIsZero $ TmSucc TmZero) TmFalse TmTrue) `shouldBe`
      Right TmTrue

    it "cannot eval (if 0 true true)" $
      eval (TmIf TmZero TmTrue TmTrue) `shouldBe`
      Left (TypeError TyBool "(if (x) <term> <term>)" TmZero)

  describe "Terminate-type" $ do
    describe "can display own expression" $ do

      it "NoRuleApplies display simple signal" $
        toDisplay NoRuleApplies `shouldBe` "[TERMINATE]"

      it "TypeError display own detail" $
        toDisplay (TypeError TyNat "(succ x)" TmTrue) `shouldBe`
        "[ERROR] TypeError of (succ x) \nRequire (x:Nat Type) \nBut true"
