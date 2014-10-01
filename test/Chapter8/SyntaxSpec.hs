{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chapter8.SyntaxSpec where

import Control.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Display
import Data.Info
import Chapter8.Syntax

mock :: (Info -> a) -> a
mock x = x Unknown

(?+) :: (Info -> a -> b) -> a -> b
(?+) x y = mock x $ y

(?*) :: (Info -> a -> b) -> (Info -> a) -> b
(?*) x y = mock x $ mock y

infixr 6 ?+
infixr 8 ?*

instance Arbitrary Info where
  arbitrary = pure Unknown

instance Arbitrary Term where
  arbitrary = oneof [gBool, gNum, gIf]
    where
    gBoolVal = ($) <$> elements [TmTrue, TmFalse] <*> arbitrary
    gNumVal  = pure $ mock TmZero
    gNumFun  = elements $ map mock [TmSucc, TmPred]
    gNumApp  = foldr ($) <$> gNumVal <*> listOf1 gNumFun
    gIsZero  = (TmIsZero ?+) <$> gNumApp
    gIf      = oneof [gIfBool, gIfNum]
    gIfBool  = (TmIf ?+) <$> gBool <*> gBool <*> gBool
    gIfNum   = (TmIf ?+) <$> gBool <*> gNum  <*> gNum
    gBool    = oneof [gBoolVal, gIsZero, gIfBool]
    gNum     = oneof [gNumVal,  gNumApp, gIfNum]

newtype WrongTerm = MkWrongTerm { unWrong :: Term }
  deriving (Eq, Show)

beWrong :: Term -> WrongTerm
beWrong = MkWrongTerm

instance Arbitrary WrongTerm where
  arbitrary = oneof [gWrongNumApp]
    where
    gBool = elements $ map mock [TmTrue, TmFalse]
    gWrongNumApp = beWrong . (TmIsZero ?+) <$> gBool

spec :: Spec
spec = do
  describe "Term data-type" $ do

    describe "as an instance of Eq type-class" $ do

      prop "A == B ==> B == A" $ \((a, b) :: (Term, Term)) ->
        a == b `shouldBe` b == a

      prop "A /= B ==> B /= A" $ \((a, b) :: (Term, Term)) ->
        a /= b `shouldBe` b /= a

    describe "as an instance of Show type-class" $ do

      prop "show" $ \(err :: Term) ->
        showList [err] `seq` showsPrec 0 err `seq` show err `seq` True

    describe "as an instance of HasType type-class" $ do

      it "typeof TmTrue => TyBool" $
        typeof (mock TmTrue) `shouldBe` Right TyBool

      it "typeof TmFalse => TyBool" $
        typeof (mock TmFalse) `shouldBe` Right TyBool

      it "typeof TmZero => TyNat" $
        typeof (mock TmZero) `shouldBe` Right TyNat

      it "typeof (TmSucc TmZero) => TyNat" $
        typeof (TmSucc ?* TmZero) `shouldBe` Right TyNat

      it "typeof (TmPred $ TmSucc TmZero) => TyNat" $
        typeof (TmPred ?+ TmSucc ?* TmZero) `shouldBe` Right TyNat

      prop "typeof (TmSucc $ [TYPE_ERROR_TERM]) => TypeError" $
        \(t :: WrongTerm) -> do
          typeof (TmSucc ?+ unWrong t) `shouldBe`
            Left (MismatchWithRequire TyNat TyBool)

      it "typeof (TmIsZero TmZero) => TyBool" $
        typeof (TmIsZero ?* TmZero) `shouldBe` Right TyBool

      it "typeof (TmIsZero TmTrue) => TypeError" $
        typeof (TmIsZero ?* TmTrue) `shouldBe`
          Left (MismatchWithRequire TyNat TyBool)

      it "typeof (TmIf (TmIsZero TmZero) TmTrue TmFalse)" $ do
        let term = (mock TmIf) (TmIsZero ?* TmZero) (mock TmTrue) (mock TmFalse)
        typeof term `shouldBe` Right TyBool

      it "typeof (TmIf (TmIsZero TmZero) TmTrue TmFalse)" $ do
        let term = (mock TmIf) (mock TmTrue) (TmSucc ?* TmZero) (mock TmFalse)
        typeof term `shouldBe`
          Left (MultiTypeReturn [("then", TyNat), ("else", TyBool)])

      prop "typof (TmIf [TypeSafe] [TYPE_ERROR_TERM] [TypeSafe])" $
        \(e :: WrongTerm) -> do
          let term = (mock TmIf) (mock TmTrue) (unWrong e) (mock TmTrue)
          typeof term `shouldBe`
            Left (MismatchWithRequire TyNat TyBool)

      prop "typof (TmIf [TypeSafe] [TypeSafe] [TYPE_ERROR_TERM])" $
        \(e :: WrongTerm) -> do
          let term = (mock TmIf) (mock TmTrue) (mock TmTrue) (unWrong e)
          typeof term `shouldBe`
            Left (MismatchWithRequire TyNat TyBool)

    describe "as an instance of Display type-class" $ do

      it "toDisplay TmTrue => \"true\"" $
        toDisplay (mock TmTrue) `shouldBe` "true"

      it "toDisplay TmFalse => \"false\"" $
        toDisplay (mock TmFalse) `shouldBe` "false"

      it "toDisplay TmZero => \"0\"" $
        toDisplay (mock TmZero) `shouldBe` "0"

      it "toDisplay (TmIsZero TmZero) => \"(zero? 0)\"" $ do
        let term = TmIsZero ?* TmZero
        toDisplay term `shouldBe` "(zero? 0)"

      it "toDisplay (TmSucc TmZero) => \"1\"" $ do
        let term = TmSucc ?* TmZero
        toDisplay term `shouldBe` "1"

      it "toDisplay (TmSucc $ TmSucc TmZero) => \"2\"" $ do
        let term = TmSucc ?+ TmSucc ?* TmZero
        toDisplay term `shouldBe` "2"

      it "toDisplay (TmSucc $ TmPred TmZero) => \"(succ (pred 0))\"" $ do
        let term = TmSucc ?+ TmPred ?* TmZero
        toDisplay term `shouldBe` "(succ (pred 0))"

      it "toDisplay (TmIf TmTrue TmZero (TmSucc TmZero)) => \"(if true 0 1)\"" $ do
        let term = (mock TmIf) (mock TmTrue) (mock TmZero) (TmSucc ?* TmZero)
        toDisplay term `shouldBe` "(if true 0 1)"
