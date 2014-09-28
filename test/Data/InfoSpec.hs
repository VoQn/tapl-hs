{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.InfoSpec where

import Control.Applicative

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Display
import Data.Info

instance Arbitrary Info where
  arbitrary = oneof [pure Unknown, genDummy]
    where
      genDummy  = FileImput <$> genName <*> genLine <*> genColumn
      genName   = elements ["<stdin>", "math.t", "list.t"]
      genLine   = elements [1..5000]
      genColumn = elements [1..10000]

spec :: Spec
spec = do
  describe "Info data-type" $ do

    describe "has file-imput information" $ do
      let info = FileImput { name = "list.t", line = 10, column = 23 }

      it "can get file-name" $
        name info `shouldBe` "list.t"

      it "can get line count" $
        line info `shouldBe` 10

      it "can get column count" $
        column info `shouldBe` 23

    describe "as an instance of Eq type-class" $ do

      prop "A == B ==> B == A" $ \((a, b) :: (Info, Info)) ->
        a == b `shouldBe` b == a

      prop "A /= B ==> B /= A" $ \((a, b) :: (Info, Info)) ->
        a /= b `shouldBe` b /= a

    describe "as an instance of Show type-class" $ do

      prop "show" $ \(info :: Info) ->
        showList [info] `seq` showsPrec 0 info `seq` show info `seq` True

    describe "as an instance of Display type-class" $ do
      it "is unknown" $
        toDisplay Unknown `shouldBe` "unknown"

      it "is a file-input" $ do
        let info = FileImput { name = "math.t", line = 3, column = 1 }
        toDisplay info `shouldBe`
          "file: math.t (line: 3, column: 1)"
