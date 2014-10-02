{-# LANGUAGE OverloadedStrings #-}
module Data.DisplaySpec where

import Test.Hspec

import Data.Display

spec :: Spec
spec = do
  describe "All instance of Display type-class" $
    it "outPut as Lazy-Text" $
      display ("Hello, World!" :: String)

  describe "Char is instance of Display type-class" $
    it "'c' -> \"c\"" $
      toDisplay 'c' `shouldBe` "c"

  describe "String is instance of Display type-class" $
    it "\"Hello\" -> \"Hello\"" $
      toDisplay ("Hello" :: String) `shouldBe` "Hello"

  describe "Int is instance of Display type-class" $
    it "100 -> \"100\"" $
      toDisplay (100 :: Int) `shouldBe` "100"

  describe "Int is instance of Display type-class" $
    it "100 -> \"100\"" $
      toDisplay (100 :: Integer) `shouldBe` "100"

  describe "Text Build Combinator" $
    it "spaceSep []" $
      spaceSep ([] :: [String]) `shouldBe` ""
