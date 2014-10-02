{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SimpleBool.TypeSpec where

import Control.Applicative

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- import Data.Display
import SimpleBool.Type

instance Arbitrary Type where
  arbitrary = oneof [genBool, genArrow]
    where
    genBool  = pure TyBool
    genArrow = foldl (TyArr) (TyBool) <$> listOf1 genBool

spec :: Spec
spec = do
  describe "SimpleBool Type data-type" $ do
    describe "as an instance of Eq type-class" $ do

      prop "A == B ==> B == A" $ \((a, b) :: (Type, Type)) ->
        a == b `shouldBe` b == a

      prop "A /= B ==> B /= A" $ \((a, b) :: (Type, Type)) ->
        a /= b `shouldBe` b /= a
