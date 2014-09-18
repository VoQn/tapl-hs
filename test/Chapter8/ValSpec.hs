{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chapter8.ValSpec where

import Control.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Chapter8.NumValSpec()
import Chapter8

instance Arbitrary Val where
  arbitrary = oneof [gBool, gNum]
    where
    gBool = elements [ValTrue, ValFalse]
    gNum  = ValNum <$> arbitrary

spec :: Spec
spec = do
  describe "Val data-type" $ do

    describe "as an instance of Eq type-class" $ do

      prop "A == B ==> B == A" $ \((a, b) :: (Val, Val)) ->
        a == b `shouldBe` b == a

      prop "A /= B ==> B /= A" $ \((a, b) :: (Val, Val)) ->
        a /= b `shouldBe` b /= a

    describe "as an instance of Show type-class" $ do

      prop "show" $ \(v :: Val) ->
        showList [v] `seq` showsPrec 0 v `seq` show v `seq` True

    describe "as an instance of Display type-class" $ do

      it "display ValTrue" $
        toDisplay ValTrue `shouldBe` "true"

      it "display ValFalse" $
        toDisplay ValFalse `shouldBe` "false"

      it "display (ValNum NumZero)" $
        toDisplay (ValNum NumZero) `shouldBe` "0"

      it "display (ValNum $ NumSucc NumZero)" $
        toDisplay (ValNum $ NumSucc NumZero) `shouldBe` "1"

    describe "as an instance of HasType type-class" $ do

      it "typeof ValTrue => Bool" $
        typeof ValTrue `shouldBe` Right TyBool

      it "typeof ValFalse => Bool" $
        typeof ValFalse `shouldBe` Right TyBool

      it "typeof (ValNum NumZero) => Nat" $
        typeof (ValNum NumZero) `shouldBe` Right TyNat
