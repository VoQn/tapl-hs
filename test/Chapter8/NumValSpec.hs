{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chapter8.NumValSpec where

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

    describe "as an instance of Show type-class" $ do

      prop "show" $ \(nv :: NumVal) ->
        showList [nv] `seq` showsPrec 0 nv `seq` show nv `seq` True

    describe "can convert real integer" $ do

      it "NumZero -> 0" $
        realNum NumZero `shouldBe` 0

      it "(NumSucc NumZero) -> 1" $
        realNum (NumSucc NumZero) `shouldBe` 1

      it "(NumSucc $ NumSucc NumZero) -> 2" $
        realNum (NumSucc $ NumSucc NumZero) `shouldBe` 2

      it "(NumSucc $ NumSucc $ NumSucc NumZero) -> 3" $
        realNum (NumSucc $ NumSucc $ NumSucc NumZero) `shouldBe` 3

    describe "as an instance of Display type-class" $ do

      it "display NumZero -> \"0\"" $
        toDisplay NumZero `shouldBe` "0"

      it "display (NumSucc NumZero) -> \"1\"" $
        toDisplay (NumSucc NumZero) `shouldBe` "1"

      it "display (NumSucc $ NumSucc NumZero) -> \"2\"" $
        toDisplay (NumSucc $ NumSucc NumZero) `shouldBe` "2"

      it "display (NumSucc $ NumSucc $ NumSucc NumZero) -> \"3\"" $
        toDisplay (NumSucc $ NumSucc $ NumSucc NumZero) `shouldBe` "3"
