module Chapter8.ParserSpec where

import Control.Monad (unless)
import Test.Hspec
import Test.HUnit.Base (assertFailure)

import Text.ParserSpec()

import Chapter8.Syntax
import Chapter8.Parser
import Chapter8.SyntaxSpec hiding (spec)

looksLike :: Term -> Term -> Bool
looksLike a b = case (a, b) of
  (TmTrue  _, TmTrue  _) -> True
  (TmFalse _, TmFalse _) -> True
  (TmZero  _, TmZero  _) -> True
  (TmIsZero _ t1, TmIsZero _ t2) -> looksLike t1 t2
  (TmSucc   _ t1, TmSucc   _ t2) -> looksLike t1 t2
  (TmPred   _ t1, TmPred   _ t2) -> looksLike t1 t2
  (TmIf _ p1 t1 f1, TmIf _ p2 t2 f2) ->
    (looksLike p1 p2) && (looksLike t1 t2) && (looksLike f1 f2)
  (_, _) -> False

shouldSame :: (Show a, Eq a) => Either a Term -> Either a Term -> Expectation
shouldSame expected actual = case (expected, actual) of
  (Left a, Left b) ->
    a `shouldBe` b
  (Right a, Right b) ->
    unless (a `looksLike` b) (assertFailure msg)
  (_, _) ->
    assertFailure msg
  where
  msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual

spec :: Spec
spec = do

  describe "parseTerm <term>" $ do

    it "parse \"true\" => true" $
      parseTerm "true" `shouldSame` Right (mock TmTrue)

    it "parse \"false\" => false" $
      parseTerm "false" `shouldSame` Right (mock TmFalse)

    it "parse \"0\" => 0" $
      parseTerm "0" `shouldSame` Right (mock TmZero)

    it "parse \"1\" => (succ 0)" $
      parseTerm "1" `shouldSame` Right (TmSucc ?* TmZero)

    it "parse \"pred 1\" => (pred $ succ 0)" $
      parseTerm "pred 1" `shouldSame` Right (TmPred ?+ TmSucc ?* TmZero)

    it "parse \"pred succ 0\" => (pred $ succ 0)" $
      parseTerm "pred succ 0" `shouldSame` Right (TmPred ?+ TmSucc ?* TmZero)

    it "parse \"zero? 0\" => (zero? 0)" $
      parseTerm "zero? 0" `shouldSame` Right (TmIsZero ?* TmZero)

    it "parse \"if true true false\" => (if true true false)" $ do
      let expect = (mock TmIf) (mock TmTrue) (mock TmTrue) (mock TmFalse)
      parseTerm "if true true false" `shouldSame`　Right expect

    it "parse \"(0)\" => 0" $
      parseTerm "(0)" `shouldSame` Right (mock TmZero)

    it "parse \"if zero? 0 true false\"" $ do
      let expect = (mock TmIf) (TmIsZero ?* TmZero) (mock TmTrue) (mock TmFalse)
      parseTerm "if zero? 0 true false" `shouldSame` Right expect

    it "parse \"if zero? succ 0 succ succ 0 succ 0\"" $ do
      let expect = (mock TmIf) (TmIsZero ?+ TmSucc ?* TmZero)
                               (TmSucc ?+ TmSucc ?* TmZero)
                               (TmSucc ?* TmZero)
      parseTerm "if zero? succ 0 succ succ 0 succ 0" `shouldSame` Right expect
