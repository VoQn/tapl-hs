module Chapter8.EvalSpec where

import Test.Hspec

import Chapter8

spec :: Spec
spec = do

  describe "evaluator" $ do

    it "eval (true :: Term) => (true :: Val)" $
      eval TmTrue `shouldBe` Right ValTrue

    it "eval (false :: Term) => (false :: Val)" $
      eval TmFalse `shouldBe` Right ValFalse

    it "eval (0 :: Term) => (0 :: Val)" $
      eval TmZero `shouldBe` Right (ValNum NumZero)

    it "eval (succ 0) => (1 :: Val)" $
      eval (TmSucc TmZero) `shouldBe` Right (ValNum $ NumSucc NumZero)

    it "eval (pred 0 :: Term) => (0 :: Val)" $
      eval (TmPred TmZero) `shouldBe` Right (ValNum $ NumZero)

    it "eval (pred succ pred succ 0) => (0 :: Val)" $ do
      let term = TmPred . TmSucc . TmPred . TmSucc $ TmZero
      eval term `shouldBe` Right (ValNum NumZero)

    it "eval (pred pred pred succ succ succ 0) => (0 :: Val)" $ do
      let term = TmPred . TmPred . TmPred . TmSucc . TmSucc . TmSucc $ TmZero
      eval term `shouldBe` Right (ValNum NumZero)

    it "eval (zero? 0 :: Term) => (true :: Val)" $
      eval (TmIsZero TmZero) `shouldBe` Right ValTrue

    it "eval (zero? 1 :: Term) => (false :: Val)" $
      eval (TmIsZero $ TmSucc TmZero) `shouldBe` Right ValFalse

    it "eval (zero? (pred succ pred 0)) => (true :: Val)" $ do
      let term = TmIsZero $ TmPred $ TmSucc $ TmPred $ TmSucc TmZero
      eval term `shouldBe` Right ValTrue

    it "eval (if (zero? 0) true false) => (false :: Val)" $ do
      let term = TmIf (TmIsZero TmZero) TmTrue TmFalse
      eval term `shouldBe` Right ValTrue

  describe "comlex terms" $
    it "eval (succ (if false 0 (succ 0))) => (succ succ 0)" $ do
      let term = TmSucc $ TmIf TmFalse TmZero $ TmSucc TmZero
      eval term `shouldBe` Right (ValNum . NumSucc . NumSucc $ NumZero)

  describe "error" $ do

    it "eval (zero? true)" $ do
      eval (TmIsZero TmTrue) `shouldBe` Left (MismatchWithRequire TyNat TyBool)
