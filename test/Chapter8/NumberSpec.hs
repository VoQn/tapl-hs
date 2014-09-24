{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chapter8.NumberSpec where

import Data.Monoid
import Control.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Chapter8

instance Arbitrary NumVal where
  arbitrary = oneof [gZero, gSucc]
    where
    gZero = pure NumZero
    gSucc = foldr ($) NumZero <$> listOf (pure NumSucc)

spec :: Spec
spec = do
  describe "NumVal data-type" $ do

    describe "as an instance of Eq type-class" $ do

      it "NumZero == NumZero" $
        NumZero == NumZero `shouldBe` True

      it "(NumSucc NumZero) == (NumSucc NumZero)" $
        (NumSucc NumZero) == (NumSucc NumZero) `shouldBe` True

      it "NumZero /= (NumSucc NumZero)" $
        NumZero /= (NumSucc NumZero) `shouldBe` True

      it "(NumSucc NumZero) /= (NumSucc $ NumSucc NumZero)" $
        (NumSucc NumZero) /= (NumSucc $ NumSucc NumZero) `shouldBe` True

      prop "A == B ==> B == A" $ \((a, b) :: (NumVal, NumVal)) ->
        a == b `shouldBe` b == a

      prop "A /= B ==> B /= A" $ \((a, b) :: (NumVal, NumVal)) ->
        a /= b `shouldBe` b /= a

    describe "as an instance of Ord type-class" $ do

      it "NumZero < (NumSucc NumZero)" $
        NumZero < (NumSucc NumZero) `shouldBe` True

      it "(NumSucc $ NumSucc NumZero) > (NumSucc NumZero)" $
        (NumSucc $ NumSucc NumZero) > (NumSucc NumZero) `shouldBe` True

    describe "as an instance of Enum type-class" $ do

      it "toEnum 0 => NumZero" $
        (toEnum 0 :: NumVal) `shouldBe` NumZero

      it "toEnum 1 => (NumSucc NumZero)" $
        (toEnum 1 :: NumVal) `shouldBe` (NumSucc NumZero)

      it "fromEnum NumZero => 0" $
        fromEnum NumZero `shouldBe` 0

      it "fromEnum (NumSucc NumZero) => 1" $
        fromEnum (NumSucc NumZero) `shouldBe` 1

      it "fromEnum (NumSucc $ NumSucc NumZero) => 2" $
        fromEnum (NumSucc $ NumSucc NumZero) `shouldBe` 2

      it "succ NumZero => (NumSucc NumZero)" $
        succ NumZero `shouldBe` (NumSucc NumZero)

      it "pred NumZero => NumZero" $
        pred NumZero `shouldBe` NumZero

      it "pred (NumSucc NumZero) => NumZero" $
        pred (NumSucc NumZero) `shouldBe` NumZero

    describe "as an instance of Monoid type-class" $ do

      it "mempty => NumZero" $
        (mempty :: NumVal) `shouldBe` NumZero

      it "NumZero <> NumZero => NumZero" $
        (NumZero <> NumZero) `shouldBe` NumZero

      it "NumZero <> (NumSucc NumZero) => (NumSucc NumZero)" $
        (NumZero <> NumSucc NumZero) `shouldBe` (NumSucc NumZero)

      it "(NumSucc NumZero) <> NumZero => (NumSucc NumZero)" $
        (NumSucc NumZero <> NumZero) `shouldBe` (NumSucc NumZero)

      it "(NumSucc NumZero) <> (NumSucc NumZero) => (NumSucc $ NumSucc NumZero)" $
        (NumSucc NumZero <> NumSucc NumZero) `shouldBe`
          (NumSucc $ NumSucc NumZero)

    describe "as an instance of Show type-class" $ do

      prop "show" $ \(nv :: NumVal) ->
        showList [nv] `seq` showsPrec 0 nv `seq` show nv `seq` True

    describe "can convert real integer" $ do

      it "NumZero -> 0" $
        toRealNum NumZero `shouldBe` 0

      it "(NumSucc NumZero) -> 1" $
        toRealNum (NumSucc NumZero) `shouldBe` 1

      it "(NumSucc $ NumSucc NumZero) -> 2" $
        toRealNum (NumSucc $ NumSucc NumZero) `shouldBe` 2

      it "(NumSucc $ NumSucc $ NumSucc NumZero) -> 3" $
        toRealNum (NumSucc $ NumSucc $ NumSucc NumZero) `shouldBe` 3

    describe "as an instance of Display type-class" $ do

      it "display NumZero -> \"0\"" $
        toDisplay NumZero `shouldBe` "0"

      it "display (NumSucc NumZero) -> \"1\"" $
        toDisplay (NumSucc NumZero) `shouldBe` "1"

      it "display (NumSucc $ NumSucc NumZero) -> \"2\"" $
        toDisplay (NumSucc $ NumSucc NumZero) `shouldBe` "2"

      it "display (NumSucc $ NumSucc $ NumSucc NumZero) -> \"3\"" $
        toDisplay (NumSucc $ NumSucc $ NumSucc NumZero) `shouldBe` "3"

    describe "as an instance of HasType type-class" $

      prop "typeof (anyValue :: NumVal) => TyNat" $ \(anyValue :: NumVal) ->
        typeof anyValue `shouldBe` Right TyNat
