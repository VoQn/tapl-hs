{-# LANGUAGE OverloadedStrings #-}
module Chapter7.EvalSpec where

import Test.Hspec

import Data.Display
import Chapter7.Syntax
import Chapter7.Eval

idn :: String -> Term
idn n = TmAbs n $ TmVar 0 1

tru :: Term
tru = TmAbs "x" $ TmAbs "y" $ TmVar 1 2

fls :: Term
fls = TmAbs "x" $ TmAbs "y" $ TmVar 0 2

spec :: Spec
spec = do

  describe "isVal" $ do

    it "isVal (\\ x x) => True" $
      isVal [] (idn "x") `shouldBe` True

    it "isVal \\.0 => False" $
      isVal [] (TmVar 0 1) `shouldBe` False

  describe "eval1" $ do

    it "eval1 id => NoRuleApplies" $
      eval1 [] (idn "x") `shouldBe` Left NoRuleApplies

    it "eval1 (id id) => id" $
      eval1 [] (TmApp (idn "x") $ idn "x") `shouldBe`
      Right (idn "x")

    it "eval1 (fls $ \\ x x) => (\\ y y)" $
      eval1 [] (TmApp fls $ idn "x") `shouldBe`
      Right (idn "y")

    it "eval1 (tru $ \\ x x) => (\\ y $ \\ x x)" $
      eval1 [] (TmApp tru $ idn "x") `shouldBe`
      Right (TmAbs "y" . TmAbs "x" $ TmVar 0 2)

  describe "eval" $ do

    it "eval ((tru A) B) => A" $
      eval [] (TmApp (TmApp tru $ idn "a") $ idn "b") `shouldBe`
      Right (idn "a")

    it "eval ((fls A) B) => B" $
      eval [] (TmApp (TmApp fls $ idn "a") $ idn "b") `shouldBe`
      Right (idn "b")

    it "eval (id . id . id . id $ \\ x x) => (\\ x x)" $ do
      let idf = idn "z"
      let idx = idn "x"
      let expr = TmApp idf . TmApp idf . TmApp idf $ TmApp idf idx
      eval [] expr `shouldBe` Right idx

  describe "Terminate" $ do

    it "can display own expression" $ do
      toDisplay NoRuleApplies `shouldBe` "[TERMINATE]"
