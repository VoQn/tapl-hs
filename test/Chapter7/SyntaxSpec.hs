{-# LANGUAGE OverloadedStrings #-}
module Chapter7.SyntaxSpec where

import Test.Hspec
import Data.Monoid
import Data.Display
import Chapter7.Syntax

spec :: Spec
spec = do

  describe "untype-lambda calculus Syntax" $ do

    describe "find variable from context" $ do

      it "(\\ x x) , x -> 0" $ do
        let ctx = [("x", NameBind)]
        nameToIndex ctx "x" `shouldBe` Right 0

      it "(\\ x (\\ y x)) , x -> 1" $ do
        let ctx = [("y",NameBind), ("x", NameBind)]
        nameToIndex ctx "x" `shouldBe` Right 1

      it "(\\ x (\\ y x)) , z -> <Error>" $ do
        let ctx = [("y",NameBind), ("x", NameBind)]
        nameToIndex ctx "z" `shouldBe` Left (UnboundIdentifier "z")

    describe "find variable name from index" $ do

      it "can find exist variable from context" $ do
        let ctx = [("x",NameBind)]
        indexToName ctx 0 1 `shouldBe` Right "x"

      it "cannot find variable by out-of-context index" $ do
        let ctx = [("x",NameBind)]
        indexToName ctx 0 2 `shouldBe` Left (WrongContextLength 0 2 1)

      it "cannot find from empty context" $ do
        let ctx = []
        indexToName ctx 0 0 `shouldBe` Left (OutOfContextIndex 0)

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

  describe "Error Handling" $ do

      it "variable has wrong context-length" $
        toDisplay (OutOfContextIndex 1) `shouldBe`
        "[NOT_FOUND] Not found variable (index: 1) in this context"

      it "unbound identifier" $
        toDisplay (UnboundIdentifier "z") `shouldBe`
        "[UNBOUND_ID] Identifier z is unbound"

      it "wrong index case : (\\.1)" $
        toDisplay (WrongContextLength 1 1 0) `shouldBe`
        "[BAD_INDEX] Value has wrong index (index: 1, length: 1)\n" <>
        "[INFO] Context has (length: 0)"

  describe "shift variable index" $ do

      it "shift (\\ x x)" $
        shift 1 (TmAbs "x" $ TmVar 0 1) `shouldBe`
        TmAbs "x" (TmVar 0 2)

      it "shift (\\ x x)" $
        shift 1 (TmAbs "x" $ TmAbs "y" $ TmVar 1 2) `shouldBe`
        TmAbs "x" (TmAbs "y" $ TmVar 1 3)

      it "shift ((\\ y (\\ x y)) (\\ x x))" $ do
        let f = TmAbs "y" $ TmAbs "x" $ TmVar 1 2
        let g = TmAbs "x" $ TmVar 0 1
        shift 1 (TmApp f g) `shouldBe`
          TmApp (TmAbs "y" $ TmAbs "x" $ TmVar 1 3) (TmAbs "x" $ TmVar 0 2)

      it "shift (x)" $
        shift 10 (TmVar 0 1) `shouldBe` TmVar 0 11

      it "shuft \\.1" $
        shift 1 (TmVar 1 1) `shouldBe` TmVar 2 2
