{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.ParserSpec where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Error

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

parseBy :: Parser a -> String -> Either ParseError a
parseBy p = parse p "<test>"

spec :: Spec
spec =
  describe "parseBy" $
    it "parseBy (many digit) \"0\"" $
      parseBy (many digit) "0" `shouldBe` Right "0"
