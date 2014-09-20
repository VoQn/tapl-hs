module Chapter7.ParserSpec where

import Test.Hspec
import Text.Parsec

import Text.ParserSpec

import Chapter7.Syntax
import Chapter7.Parser

parseVar :: Context -> String -> Either ParseError Term
parseVar ctx = parseBy (pVar ctx)

parseApp :: Context -> String -> Either ParseError Term
parseApp ctx = parseBy (pApp ctx)

spec :: Spec
spec = do

  describe "Nameless, variable parsers" $ do

    it "[x, y, z] x" $ do
      let ctx = ["x", "y", "z"]
      parseVar ctx "z" `shouldBe` Right (2 <+ 3)

    it "[z, y, z] (x y)" $ do
      let ctx = ["z", "y", "x"]
      parseApp ctx "(x y)" `shouldBe` Right ((2 <+ 3) <+> (1 <+ 3))

    it "[y, x] (v)" $ do
      let ctx = ["y", "x"]
      parseVar ctx "v" `shouldBe` Right (TmFree "v")

    it "[z, y, x] (w x y z)" $ do
      let ctx = ["z", "y", "x"]
      parseApp ctx "(w x y z)" `shouldBe`
        Right (TmFree "w" <+> (2 <+ 3) <+> (1 <+ 3) <+> (0 <+ 3))

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

    it "(\\ x (\\ y y))" $
      parseExpr "(\\ x (\\ y y))" `shouldBe`
      Right ("x" +> "y" +> 0 <+ 2)

    it "(\\ x (\\ y (x y)))" $
      parseExpr "(\\ x (\\ y (x y)))" `shouldBe`
      Right ("x" +> "y" +> (1 <+ 2) <+> (0 <+ 2))

  describe "primitive functions" $ do

    it "(id)" $
      parseExpr "id" `shouldBe` Right (TmFree "id")

    it "(tru)" $
      parseExpr "tru" `shouldBe` Right (TmFree "tru")

    it "(fls)" $
      parseExpr "fls" `shouldBe` Right (TmFree "fls")

    it "(test)" $
      parseExpr "tst" `shouldBe` Right (TmFree "tst")

    it "(and)" $
      parseExpr "and" `shouldBe` Right (TmFree "and")

    it "(or)" $
      parseExpr "or" `shouldBe` Right (TmFree "or")

  describe "parse function call" $ do

    it "(((tst tru) id) id)" $
      parseExpr "(((tst tru) id) id)" `shouldBe`
      Right (TmFree "tst" <+> TmFree "tru" <+> TmFree "id" <+> TmFree "id")

    it "(test tru id id)" $
      parseExpr "(tst tru id id)" `shouldBe`
      Right (TmFree "tst" <+> TmFree "tru" <+> TmFree "id" <+> TmFree "id")
