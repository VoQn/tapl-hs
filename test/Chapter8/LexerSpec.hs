module Chapter8.LexerSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Text.ParserSpec

import Text.Parsec
import Chapter8.Lexer

spec :: Spec
spec = do

  describe "number-token-parser" $ do

    it "get token from \"0\"" $
      parseBy tNumber "0" `shouldBe` Right 0

    it "get token from \"1\"" $
      parseBy tNumber "1" `shouldBe` Right 1

    it "get token from \"10\"" $
      parseBy tNumber "10" `shouldBe` Right 10

    prop "get token from `any natural-integer-number`" $
      \i -> i >= 0 ==>
        parseBy tNumber (show i) `shouldBe` Right i

  describe "parens-parser-combinator" $ do

    it "\"(10)\"" $
      parseBy (tParens tNumber) "(10)" `shouldBe` Right 10

  describe "lexeme-parser-combinator" $ do

    it "(tLexeme tNumber) get tokens from \"1 2 3 4 5\"" $
      parseBy (many $ tLexeme tNumber) "1 2 3 4 5" `shouldBe`
        Right [1, 2, 3, 4, 5]

    it "(tLexeme tNumber) get tokens from \"1 2 3 4 5　\"" $
      parseBy (many $ tLexeme tNumber) "1 2 3 4 5　" `shouldBe`
        Right [1, 2, 3, 4, 5]

  describe "reserved-token-parser" $ do

    it "\"true\"" $
      parseBy (tReserved "true") "true" `shouldBe` Right ()

  describe "reserved-operator-token-parser" $ do

    it "\"$\"" $
      parseBy (tReservedOp "$") "$" `shouldBe` Right ()
