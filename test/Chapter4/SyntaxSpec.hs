{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chapter4.SyntaxSpec where

import Control.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Display
import Chapter4.Syntax

instance Arbitrary Term where
  arbitrary = oneof [gNum, gBool, gIf]
    where
    gZero  = elements [TmZero]
    gBool1 = elements [TmTrue, TmFalse]

    gNumV = foldr ($) TmZero <$> listOf (elements [TmSucc, TmPred])

    gIsZero = TmIsZero <$> gNumV
    gIfBool = TmIf <$> gBool <*> gBool <*> gBool
    gIfNum  = TmIf <$> gBool <*> gNum  <*> gNum

    gIf   = oneof [gIfNum, gIfBool]
    gNum  = oneof [gZero,  gNumV]
    gBool = oneof [gBool1, gIsZero]

instance Arbitrary Ty where
  arbitrary = elements [TyBool,TyNat]

spec :: Spec
spec = do
  describe "Term can describe own that it is numeric-value or not" $ do

    it "0 (zero)" $
      isNumeric TmZero `shouldBe` True

    it "(succ 0)" $
      isNumeric (TmSucc TmZero) `shouldBe` True

    it "(pred 0)" $
      isNumeric (TmPred TmZero) `shouldBe` True

    it "true" $
      isNumeric TmTrue `shouldBe` False

    it "false" $
      isNumeric TmFalse `shouldBe` False

    it "(zero? 0)" $
      isNumeric (TmIsZero TmZero) `shouldBe` False

    it "(if (zero? 0) 0 (succ 0))" $
      isNumeric (TmIf (TmIsZero TmZero) TmZero (TmSucc TmZero)) `shouldBe` True

    it "(if true true false)" $
      isNumeric (TmIf TmTrue TmTrue TmFalse) `shouldBe` False

  describe "Term can describe own that it is bool-value or not" $ do

    it "0 (zero)" $
      isBool TmZero `shouldBe` False

    it "(succ 0)" $
      isBool (TmSucc TmZero) `shouldBe` False

    it "(pred 0)" $
      isBool (TmPred TmZero) `shouldBe` False

    it "true" $
      isBool TmTrue `shouldBe` True

    it "false" $
      isBool TmFalse `shouldBe` True

    it "(zero? 0)" $
      isBool (TmIsZero TmZero) `shouldBe` True

    it "(if (zero? 0) 0 (succ 0))" $
      isBool (TmIf (TmIsZero TmZero) TmZero (TmSucc TmZero)) `shouldBe` False

    it "(if true true false)" $
      isBool (TmIf TmTrue TmTrue TmFalse) `shouldBe` True

  describe "Term can display own expression" $ do

    it "0 (zero)" $
      toDisplay TmZero `shouldBe` "0"

    it "(succ 0)" $
      toDisplay (TmSucc TmZero) `shouldBe` "(succ 0)"

    it "(pred 0)" $
      toDisplay (TmPred TmZero) `shouldBe` "(pred 0)"

    it "true" $
      toDisplay TmTrue `shouldBe` "true"

    it "false" $
      toDisplay TmFalse `shouldBe` "false"

    it "(zero? 0)" $
      toDisplay (TmIsZero TmZero) `shouldBe` "(zero? 0)"

    it "(if (zero? 0) 0 (succ 0))" $
      toDisplay (TmIf (TmIsZero TmZero) TmZero (TmSucc TmZero)) `shouldBe`
      "(if (zero? 0) 0 (succ 0))"

    it "(if true true false)" $
      toDisplay (TmIf TmTrue TmTrue TmFalse) `shouldBe`
      "(if true true false)"

  describe "Term can compare each as equal" $ do
    prop "x == x" propTermEq
    prop "x /= y ==> x /= y" propTermNotEq

  describe "Term can show own" $ do

    it "TmZero"  $ show TmZero  `shouldBe` "TmZero"
    it "TmTrue"  $ show TmTrue  `shouldBe` "TmTrue"
    it "TmFalse" $ show TmFalse `shouldBe` "TmFalse"

    it "TmIsZero TmZero" $
      show (TmIsZero TmZero) `shouldBe` "TmIsZero TmZero"

    it "TmSucc TmZero" $
      show (TmSucc TmZero) `shouldBe` "TmSucc TmZero"

    it "TmPred TmZero" $
      show (TmPred TmZero) `shouldBe` "TmPred TmZero"

    it "TmIf TmTrue TmZero (TmSucc TmZero)" $
      show (TmIf TmTrue TmZero (TmSucc TmZero)) `shouldBe`
      "TmIf TmTrue TmZero (TmSucc TmZero)"

  describe "Type can compare each as equal" $ do

    prop "x == x" propTyEq
    prop "x /= y ==> x /= y" propTyNotEq

  describe "Type can display own expression" $ do

    it "Bool" $ toDisplay TyBool `shouldBe` "Bool"
    it "Nat"  $ toDisplay TyNat  `shouldBe` "Nat"

  describe "Type can show own" $ do

    it "Bool" $ show TyBool `shouldBe` "TyBool"
    it "Nat"  $ show TyNat  `shouldBe` "TyNat"

propTyEq :: Ty -> Bool
propTyEq x = x == x

propTyNotEq :: Ty -> Ty -> Property
propTyNotEq x y = x /= y ==> x /= y

propTermEq :: Term -> Bool
propTermEq x = x == x

propTermNotEq :: Term -> Term -> Property
propTermNotEq x y = x /= y ==> x /= y
