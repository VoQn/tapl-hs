{-# LANGUAGE OverloadedStrings #-}
module SimpleBool.ErrorSpec where

import Test.Hspec

import Data.Display
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
