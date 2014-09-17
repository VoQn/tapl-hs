{-# LANGUAGE OverloadedStrings #-}
module Chapter7.EvalSpec where

import Test.Hspec

import Data.Display
import Chapter7.Syntax
import Chapter7.Eval

spec :: Spec
spec = do

  describe "isVal" $ do

    it "isVal (\\ x x) => True" $
      isVal [] (cIdn "x") `shouldBe` True

    it "isVal \\.0 => False" $
      isVal [] (0 <+ 1) `shouldBe` False

    it "isVal (id) => isVal (\\ x x) => True" $
      isVal [] (TmFree "id") `shouldBe` True

    it "isVal (undefinedFunc) => isVal #undefined => False" $
      isVal [] (TmFree "undefinedFunc") `shouldBe` False

  describe "eval1" $ do

    it "eval1 id => NoRuleApplies" $
      eval1 [] cId `shouldBe` Left NoRuleApplies

    it "eval1 (id id) => id" $
      eval1 [] (cId <+> cId) `shouldBe`
      Right cId

    it "eval1 (fls $ \\ x x) => (\\ f f)" $
      eval1 [] (cFls <+> cId) `shouldBe`
      Right (cIdn "f")

    it "eval1 (tru $ \\ x x) => (\\ f $ \\ x x)" $
      eval1 [] (cTru <+> cId) `shouldBe`
      Right ("f" +> 1 -^ cId)

  describe "eval" $ do

    describe "(id)" $ do

      it "eval (id) => id" $
        eval [] (TmFree "id") `shouldBe` Right cId

      it "eval (id (id (id (id (\\ x x))))) => (\\ x x)" $ do
        let term = cIdn "a" <+> cIdn "b" <+> cIdn "c" <+> cIdn "d" <+> cIdn "x"
        eval [] term `shouldBe` Right (cIdn "x")

    describe "tru/fls" $ do

      it "eval (tru) => tru" $
        eval [] (TmFree "tru") `shouldBe` Right cTru

      it "eval (fls) => fls" $
        eval [] (TmFree "fls") `shouldBe` Right cFls

      it "eval (tru A B) => A" $ do
        let term = cTru <+> cIdn "a" <+> cIdn "b"
        eval [] term `shouldBe` Right (cIdn "a")

      it "eval (fls A B) => B" $ do
        let term = cFls <+> cIdn "a" <+> cIdn "b"
        eval [] term `shouldBe` Right (cIdn "b")

    describe "(tst)" $ do
      it "eval (tst) => test" $ do
        eval [] (TmFree "tst") `shouldBe` Right cTst

      it "eval (tst tru v w) => v" $ do
        let term = cTst <+> cTru <+> cIdn "v" <+> cIdn "w"
        eval [] term `shouldBe` Right (cIdn "v")

      it "eval (tst fls v w) => w" $ do
        let term = cTst <+> cFls <+> cIdn "v" <+> cIdn "w"
        eval [] term `shouldBe` Right (cIdn "w")

    describe "(and)" $ do

      it "eval (and) => and" $
        eval [] (TmFree "and") `shouldBe` Right cAnd

      it "eval (and tru tru) => tru" $ do
        let expr = cAnd <+> cTru <+> cTru
        eval [] expr `shouldBe` Right cTru

      it "eval (and fls tru) => fls" $ do
        let expr = cAnd <+> cFls <+> cTru
        eval [] expr `shouldBe` Right cFls

      it "eval (and tru fls) => fls" $ do
        let expr = cAnd <+> cTru <+> cFls
        eval [] expr `shouldBe` Right cFls

      it "eval (and fls fls) => fls" $ do
        let expr = cAnd <+> cFls <+> cFls
        eval [] expr `shouldBe` Right cFls

    describe "(or)" $ do

      it "eval (or) => or" $ do
        eval [] (TmFree "or") `shouldBe` Right cOr

      it "eval (or tru tru) => tru" $ do
        let expr = cOr <+> cTru <+> cTru
        eval [] expr `shouldBe` Right cTru

      it "eval (or fls tru) => tru" $ do
        let expr = cOr <+> cFls <+> cTru
        eval [] expr `shouldBe` Right cTru

      it "eval (or tru fls) => tru" $ do
        let expr = cOr <+> cTru <+> cFls
        eval [] expr `shouldBe` Right cTru

      it "eval (or fls fls) => fls" $ do
        let expr = cOr <+> cFls <+> cFls
        eval [] expr `shouldBe` Right cFls

    describe "(pir)/(fst)/(snd)" $ do

      it "eval (pir) => pair" $
        eval [] (TmFree "pir") `shouldBe` Right cPir

      it "eval (fst) => fst" $
        eval [] (TmFree "fst") `shouldBe` Right cFst

      it "eval (snd) => snd" $
        eval [] (TmFree "snd") `shouldBe` Right cSnd

      it "eval (fst (pir (\\ v v) (\\ w w))) => (\\ v v)" $ do
        let term = cFst <+> (cPir <+> cIdn "v" <+> cIdn "w")
        eval [] term `shouldBe` Right (cIdn "v")

      it "eval (snd (pir (\\ v v) (\\ w w))) => (\\ w w)" $ do
        let term = cSnd <+> (cPir <+> cIdn "v" <+> cIdn "w")
        eval [] term `shouldBe` Right (cIdn "w")

    describe "(zro)/(one)/(scc)/(zro?)" $ do

      it "eval (zro) => zero" $
        eval [] (TmFree "zro") `shouldBe` Right cZro

      it "eval (one) => one" $
        eval [] (TmFree "one") `shouldBe` Right cOne

      it "eval (scc) => succ" $
        eval [] (TmFree "scc") `shouldBe` Right cScc

      it "eval (zero?) => zero?" $
        eval [] (TmFree "zro?") `shouldBe` Right cIsZro

      it "eval (zero? zero) => tru" $
        eval [] (cIsZro <+> cZro) `shouldBe` Right cTru

      it "eval (zero? (succ zero)) => fls" $ do
        let term = cIsZro <+> (cScc <+> cZro)
        eval [] term `shouldBe` Right cFls

  describe "Terminate" $ do

    it "can display own expression" $ do
      toDisplay NoRuleApplies `shouldBe` "[TERMINATE]"
