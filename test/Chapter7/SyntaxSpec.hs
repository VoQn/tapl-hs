{-# LANGUAGE OverloadedStrings #-}
module Chapter7.SyntaxSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Monoid
import Data.Display
import Chapter7.Syntax
import Chapter7.Exception

spec :: Spec
spec = do

  describe "untype-lambda calculus Syntax" $ do

    describe "find variable from context" $ do

      it "(\\ x x) , x -> 0" $ do
        let ctx = ["x"]
        nameToIndex ctx "x" `shouldBe` Right 0

      it "(\\ x (\\ y x)) , x -> 1" $ do
        let ctx = ["y", "x"]
        nameToIndex ctx "x" `shouldBe` Right 1

      it "(\\ x (\\ y x)) , z -> <Error>" $ do
        let ctx = ["y", "x"]
        nameToIndex ctx "z" `shouldBe` Left (UnboundIdentifier "z")

    describe "find variable name from index" $ do

      it "can find exist variable from context" $ do
        let ctx = ["x"]
        indexToName ctx 0 1 `shouldBe` Right "x"

      it "cannot find variable by out-of-context index" $ do
        let ctx = ["x"]
        indexToName ctx 0 2 `shouldBe` Left (WrongContextDepth 0 2 1)

      it "cannot find from empty context" $ do
        let ctx = []
        indexToName ctx 0 0 `shouldBe` Left (OutOfContextIndex 0)

    describe "display own expression according to context" $ do

      it "id : (\\ x x)" $
        toDisplay ("x" +> 0 <+ 1) `shouldBe` "(\\ x x)"

      it "seq : (\\ y (\\ x y))" $
        toDisplay ("y" +> "x" +> 1 <+ 2) `shouldBe`
          "(\\ y (\\ x y))"

      it "tru : (\\ y (\\ x x))" $
        toDisplay ("y" +> "x" +> 0 <+ 2) `shouldBe`
          "(\\ y (\\ x x))"

      it "fls : (\\ x (\\ x x))" $
        toDisplay ("x" +> "x" +> 0 <+ 2) `shouldBe`
          "(\\ x (\\ x' x'))"

      it "apply : ((\\ x x) (\\ y y))" $ do
        let idt1 = "x" +> 0 <+ 1
        let idt2 = "y" +> 0 <+ 1
        toDisplay (idt1 <+> idt2) `shouldBe`
          "((\\ x x) (\\ y y))"

      it "free var \"foo\"" $ do
        toDisplay (TmFree "foo") `shouldBe` "foo"

    describe "Error Handling" $ do

      it "variable has wrong context-length" $
        toDisplay (OutOfContextIndex 1) `shouldBe`
        "[NOT_FOUND] Not found variable (index: 1) in this context"

      it "unbound identifier" $
        toDisplay (UnboundIdentifier "z") `shouldBe`
        "[UNBOUND_ID] Identifier \"z\" is unbound"

      it "wrong index case : (\\.1)" $
        toDisplay (WrongContextDepth 1 1 0) `shouldBe`
        "[BAD_INDEX] Value has wrong index (index: 1, length: 1)\n" <>
        "[INFO] Context has (length: 0)"

    describe "shift variable index" $ do

      it "shift (\\ x x)" $
        shift 1 ("x" +> 0 <+ 1) `shouldBe`
          ("x" +> 0 <+ 2)

      it "shift 1 (\\ x x)" $
        shift 1 ("x" +> "y" +> 1 <+ 2) `shouldBe`
          ("x" +> "y" +> 1 <+ 3)

      it "shift 1 ((\\ y (\\ x y)) (\\ x x))" $ do
        let f = "y" +> "x" +> 1 <+ 2
        let g = "x" +> 0 <+ 1
        shift 1 (f <+> g) `shouldBe`
          (("y" +> "x" +> 1 <+ 3) <+> ("x" +> 0 <+ 2))

      it "shuft 1 \\.1" $
        shift 1 (1 <+ 2) `shouldBe` (2 <+ 3)

      it "shift 2 \\.2" $
        shift 1 ("x" +> "y" +> "z" +> 2 <+ 3) `shouldBe`
          ("x" +> "y" +> "z" +> 2 <+ 4)

      it "shift 10 \\.0" $
        shift 10 (0 <+ 1) `shouldBe` (10 <+ 11)

      prop "shift x (free v)" $ \n ->
        shift n (TmFree "v") `shouldBe` (TmFree "v")

    describe "substitute varibales" $ do

      it "subst (((\\ x x) (\\ x x)) (\\ y y))" $ do
        let f = "x" +> 0 <+ 1
        let g = "x" +> 0 <+ 1
        let h = "y" +> 0 <+ 1
        subst 1 h (f <+> g) `shouldBe` (f <+> g)

      it "subst 0 \\.0 \\.0" $
        subst 0 (0 <+ 1) (0 <+ 1) `shouldBe` (0 <+ 1)

      it "subst 1 \\.2(4) \\.1(5)" $
        subst 1 (2 <+ 4) (1 <+ 5) `shouldBe` (2 <+ 4)

      prop "subst x (free v)" $ \x ->
        subst x (0 <+ 1) (TmFree "v") `shouldBe` (TmFree "v")

    describe "substitute on Top Level" $ do

      it "substTop ((\\ x (\\ y x)) (\\ x x)) <-> (\\ z z)" $ do
        let t = "z" +> 0 <+ 1
        let f = "x" +> "y" +> 1 <+ 2
        let g = "x" +> 0 <+ 1

        substTop (f <+> g) t `shouldBe` ("z" +> 0 <+ 0)
