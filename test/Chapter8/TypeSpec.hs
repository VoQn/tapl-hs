{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chapter8.TypeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Chapter8

instance Arbitrary Ty where
  arbitrary = elements [TyBool, TyNat]

spec :: Spec
spec = do
  describe "Ty data-type" $ do

    describe "as an instance of Eq type-class" $ do

      it "TyBool == TyBool" $
        TyBool == TyBool `shouldBe` True

      it "TyNat  == TyNat" $
        TyNat  == TyNat  `shouldBe` True

      it "TyBool /= TyNat" $
        TyBool /= TyNat  `shouldBe` True

      it "TyNat  /= TyBool" $
        TyNat  /= TyBool `shouldBe` True

      prop "A == B ==> B == A" $ \((a, b) :: (Ty, Ty)) ->
        a == b `shouldBe` b == a

      prop "A /= B ==> B /= A" $ \((a, b) :: (Ty, Ty)) ->
        a /= b `shouldBe` b /= a

    describe "as an instance of Show type-class" $ do

      prop "show" $ \(ty :: Ty) ->
        showList [ty] `seq` showsPrec 0 ty `seq` show ty `seq` True

    describe "as an instance of Display type-class" $ do

      it "display TyBool" $
        toDisplay TyBool `shouldBe` "Bool"

      it "display TyNat" $
        toDisplay TyNat  `shouldBe` "Nat"

    describe "as an instance of HasType type-class" $ do

      it "typeof TyBool => TyBool" $ typeof TyBool `shouldBe` Right TyBool
      it "typeof TyNat  => TyNat"  $ typeof TyNat  `shouldBe` Right TyNat
