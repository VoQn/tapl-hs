{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chapter8.SyntaxSpec where

import Control.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Chapter8

instance Arbitrary Term where
  arbitrary = oneof [gBool, gNum, gIf]
    where
    gBoolVal = elements [TmTrue, TmFalse]
    gNumVal  = pure TmZero
    gNumApp  = foldr ($) TmZero <$> listOf1 (elements [TmSucc, TmPred])
    gIsZero  = TmIsZero <$> gNumApp
    gIf      = oneof [gIfBool, gIfNum]
    gIfBool  = TmIf <$> gBool <*> gBool <*> gBool
    gIfNum   = TmIf <$> gBool <*> gNum  <*> gNum
    gBool    = oneof [gBoolVal, gIsZero, gIfBool]
    gNum     = oneof [gNumVal,  gNumApp, gIfNum]

spec :: Spec
spec = do
  describe "Term data-type" $ do

    describe "as an instance of Eq type-class" $ do

      prop "A == B ==> B == A" $ \((a, b) :: (Term, Term)) ->
        a == b `shouldBe` b == a

      prop "A /= B ==> B /= A" $ \((a, b) :: (Term, Term)) ->
        a /= b `shouldBe` b /= a

    describe "as an instance of Show type-class" $ do

      prop "show" $ \(err :: Term) ->
        showList [err] `seq` showsPrec 0 err `seq` show err `seq` True

    describe "as an instance of HasType type-class" $ do

      it "typeof TmTrue => TyBool" $ do
        typeof TmTrue `shouldBe` Right TyBool

      it "typeof TmFalse => TyBool" $ do
        typeof TmFalse `shouldBe` Right TyBool

      it "typeof TmZero => TyNat" $ do
        typeof TmZero `shouldBe` Right TyNat

      it "typeof (TmIsZero TmZero) => TyBool" $
        typeof (TmIsZero TmZero) `shouldBe` Right TyBool

      it "typeof (TmIsZero TmTrue) => TypeError" $
        typeof (TmIsZero TmTrue) `shouldBe`
          Left (MismatchWithRequire TyNat TyBool)
