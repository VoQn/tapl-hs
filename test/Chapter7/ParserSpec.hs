{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chapter7.ParserSpec where

import Test.Hspec
import Text.Parsec.Error

import Chapter7.Syntax
import Chapter7.Parser

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

spec :: Spec
spec = do
  describe "primitive functions" $ do

    it "(id)" $
      parseExpr "id" `shouldBe` Right cId

    it "(tru)" $
      parseExpr "tru" `shouldBe` Right cTru

    it "(fls)" $
      parseExpr "fls" `shouldBe` Right cFls

    it "(test)" $
      parseExpr "test" `shouldBe` Right cTest

    it "(and)" $
      parseExpr "and" `shouldBe` Right cAnd

    it "(or)" $
      parseExpr "or" `shouldBe` Right cOr

  describe "parse function call" $ do

    it "(((test tru) id) id)" $
      parseExpr "(((test tru) id) id)" `shouldBe`
      Right (cTest <+> cTru <+> cId <+> cId)

    it "(test tru id id)" $
      parseExpr "(test tru id id)" `shouldBe`
      Right (cTest <+> cTru <+> cId <+> cId)
