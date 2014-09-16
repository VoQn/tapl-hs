{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chapter7.ParserSpec where

import Test.Hspec
import Text.Parsec
import Text.Parsec.Error

import Chapter7.Syntax
import Chapter7.Parser

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

parseVar :: Context -> String -> Either ParseError Term
parseVar ctx = parse (pVar ctx) "<test>"

parseApp :: Context -> String -> Either ParseError Term
parseApp ctx = parse (pApp' ctx) "<test>"

spec :: Spec
spec = do

  describe "Nameless, variable parsers" $ do

    it "[x, y, z] x" $ do
      let ctx = [("x", NameBind), ("y", NameBind), ("z", NameBind)]
      parseVar ctx "z" `shouldBe` Right (2 <+ 3)

    it "[z, y, z] (x y)" $ do
      let ctx = [("z", NameBind), ("y", NameBind), ("x", NameBind)]
      parseApp ctx "(x y)" `shouldBe` Right ((2 <+ 3) <+> (1 <+ 3))

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

    it "(\\ (x y z) (z y x))" $
      parseExpr "(\\ (x y z) (z y x))" `shouldBe`
      Right ("x" +> "y" +> "z" +> (0 <+ 3) <+> (1 <+ 3) <+> (2 <+ 3))

    it "(\\ (x y z) (x (y z)))" $
      parseExpr "(\\ (x y z) (x (y z)))" `shouldBe`
      Right ("x" +> "y" +> "z" +> (2 <+ 3) <+> ((1 <+ 3) <+> (0 <+ 3)))

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
