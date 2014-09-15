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

  describe "lambda expression" $ do

    it "(\\ x x)" $
      parseExpr "(\\ x x)" `shouldBe`
      Right ("x" +> 0 <+ 1)

    it "(\\ (x y) y)" $
      parseExpr "(\\ (x y) y)" `shouldBe`
      Right ("x" +> "y" +> 0 <+ 2)

    it "(\\ (x y z) y)" $
      parseExpr "(\\ (x y z) y)" `shouldBe`
      Right ("x" +> "y" +> "z" +> 1 <+ 3)

    it "(\\ (x y) (x y))" $
      parseExpr "(\\ (x y) (x y))" `shouldBe`
      Right ("x" +> "y" +> (1 <+ 2) <+> (0 <+ 2))

  describe "primitive functions" $ do

    it "(id)" $
      parseExpr "id" `shouldBe` Right cId

    it "(tru)" $
      parseExpr "tru" `shouldBe` Right cTru

    it "(fls)" $
      parseExpr "fls" `shouldBe` Right cFls

    it "(test)" $
      parseExpr "tst" `shouldBe` Right cTst

    it "(and)" $
      parseExpr "and" `shouldBe` Right cAnd

    it "(or)" $
      parseExpr "or" `shouldBe` Right cOr

  describe "parse function call" $ do

    it "(((tst tru) id) id)" $
      parseExpr "(((tst tru) id) id)" `shouldBe`
      Right (cTst <+> cTru <+> cId <+> cId)

    it "(test tru id id)" $
      parseExpr "(tst tru id id)" `shouldBe`
      Right (cTst <+> cTru <+> cId <+> cId)
