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
