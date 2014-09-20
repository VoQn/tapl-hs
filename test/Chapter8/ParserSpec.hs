{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chapter8.ParserSpec where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Error (errorMessages)

import Chapter8.Syntax
import Chapter8.Parser

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

parseBy :: Parser a -> String -> Either ParseError a
parseBy p = parse p "<test>"

spec :: Spec
spec = do

  describe "parseTerm <term>" $ do

    it "parse \"true\" => true" $
      parseTerm "true" `shouldBe` Right TmTrue

    it "parse \"false\" => false" $
      parseTerm "false" `shouldBe` Right TmFalse

    it "parse \"0\" => 0" $
      parseTerm "0" `shouldBe` Right TmZero

    it "parse \"1\" => (succ 0)" $
      parseTerm "1" `shouldBe` Right (TmSucc TmZero)

    it "parse \"pred 1\" => (pred $ succ 0)" $
      parseTerm "pred 1" `shouldBe` Right (TmPred $ TmSucc TmZero)

    it "parse \"pred succ 0\" => (pred $ succ 0)" $
      parseTerm "pred succ 0" `shouldBe` Right (TmPred $ TmSucc TmZero)

    it "parse \"zero? 0\" => (zero? 0)" $
      parseTerm "zero? 0" `shouldBe` Right (TmIsZero TmZero)

    it "parse \"if true true false\" => (if true true false)" $
      parseTerm "if true true false" `shouldBe`
        Right (TmIf TmTrue TmTrue TmFalse)

    it "parse \"(0)\" => 0" $
      parseTerm "(0)" `shouldBe` Right TmZero

    it "parse \"if zero? 0 true false\"" $
      parseTerm "if zero? 0 true false" `shouldBe`
        Right (TmIf (TmIsZero TmZero) TmTrue TmFalse)

    it "parse \"if zero? succ 0 succ succ 0 succ 0\"" $
      parseTerm "if zero? succ 0 succ succ 0 succ 0" `shouldBe`
        Right (TmIf (TmIsZero $ TmSucc TmZero)
                    (TmSucc $ TmSucc TmZero)
                    (TmSucc TmZero))
