{-# LANGUAGE OverloadedStrings #-}
module Chapter7.SyntaxSpec where

import Test.Hspec

import Data.Display
import Chapter7.Syntax

spec :: Spec
spec = do

  describe "untype-lambda calculus Syntax" $ do

    describe "find variable from context" $ do

      it "(\\ x x) , x -> 0" $ do
        let ctx = [("x", NameBind)]
        (nameToIndex ctx "x") `shouldBe` 0

      it "(\\ x (\\ y x)) , x -> 1" $ do
        let ctx = [("y",NameBind), ("x", NameBind)]
        (nameToIndex ctx "x") `shouldBe` 1

    describe "display own expression according to context" $ do

      it "id : (\\ x x)" $ do
        let expr = TmAbs "x" $ TmVar 0 1
        let ctx  = []
        toDisplay (withContext ctx expr) `shouldBe`
          "(\\ x x)"

      it "seq : (\\ y (\\ x y))" $ do
        let expr = TmAbs "y" $ TmAbs "x" $ TmVar 1 2
        let ctx  = []
        toDisplay (withContext ctx expr) `shouldBe`
          "(\\ y (\\ x y))"

      it "tru : (\\ y (\\ x x))" $ do
        let expr = TmAbs "y" $ TmAbs "x" $ TmVar 0 2
        let ctx  = []
        toDisplay (withContext ctx expr) `shouldBe`
          "(\\ y (\\ x x))"

      it "fls : (\\ x (\\ x x))" $ do
        let expr = TmAbs "x" $ TmAbs "x" $ TmVar 0 2
        let ctx  = []
        toDisplay (withContext ctx expr) `shouldBe`
          "(\\ x (\\ x' x'))"

      it "apply : ((\\ x x) (\\ y y))" $ do
        let idt1 = TmAbs "x" $ TmVar 0 1
        let idt2 = TmAbs "y" $ TmVar 0 1
        let expr = TmApp idt1 idt2
        let ctx  = []
        toDisplay (withContext ctx expr) `shouldBe`
          "((\\ x x) (\\ y y))"

      it "wrong index case : (\\.1)" $ do
        let expr = TmVar 1 1
        let ctx  = []
        toDisplay (withContext ctx expr) `shouldBe`
          "[BAD INDEX]"
