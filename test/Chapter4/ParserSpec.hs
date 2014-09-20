module Chapter4.ParserSpec where

import Test.Hspec

import Text.ParserSpec()

import Chapter4.Syntax
import Chapter4.Parser

spec :: Spec
spec = do
  describe "token parser" $ do

    it "can parse \"true\" -> true" $
      parseExpr "true" `shouldBe` Right TmTrue

    it "can parse \"false\" -> false" $
      parseExpr "false" `shouldBe` Right TmFalse

    it "can parse \"0\" -> 0" $
      parseExpr "0" `shouldBe` Right TmZero

    it "can parse \"(if true false true)\" -> (if true false true)" $
      parseExpr "(if true false true)" `shouldBe`
      Right (TmIf TmTrue TmFalse TmTrue)

    it "can parse \"(succ 0)\" -> (succ 0)" $
      parseExpr "(succ 0)" `shouldBe`
      Right (TmSucc TmZero)

    it "can parse \"(pred 0)\" -> (pred 0)" $
      parseExpr "(pred 0)" `shouldBe`
      Right (TmPred TmZero)

    it "can parse \'(zero? 0)\" -> (zero? 0)" $
      parseExpr "(zero? 0)" `shouldBe`
      Right (TmIsZero TmZero)

    it "can parse \"(true)\" -> true" $
      parseExpr "(true)" `shouldBe` Right TmTrue
