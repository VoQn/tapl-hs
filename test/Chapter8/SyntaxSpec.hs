{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chapter8.SyntaxSpec where

import Control.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Chapter8

instance Arbitrary Term where
  arbitrary = oneof [gBool, gNum, gIf]
    where
    gBoolVal = elements [TmTrue, TmFalse]
    gNumVal  = pure TmZero
    gNumApp  = foldr ($) TmZero <$> listOf1 (elements [TmSucc, TmPred])
    gIsZero  = TmIsZero <$> gNumApp
    gIf      = oneof [gIfBool, gIfNum]
    gIfBool  = TmIf <$> gBool <*> gBool <*> gBool
    gIfNum   = TmIf <$> gBool <*> gNum  <*> gNum
    gBool    = oneof [gBoolVal, gIsZero, gIfBool]
    gNum     = oneof [gNumVal,  gNumApp, gIfNum]

newtype WrongTerm = MkWrongTerm { unWrong :: Term }
  deriving (Eq, Show)

beWrong :: Term -> WrongTerm
beWrong = MkWrongTerm

instance Arbitrary WrongTerm where
  arbitrary = oneof [gWrongNumApp]
    where
    gWrongNumApp = beWrong . TmIsZero <$> elements [TmTrue, TmFalse]

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
        typeof TmTrue `shouldBe` Right TyBool

      it "typeof TmFalse => TyBool" $
        typeof TmFalse `shouldBe` Right TyBool

      it "typeof TmZero => TyNat" $
        typeof TmZero `shouldBe` Right TyNat

      it "typeof (TmSucc TmZero) => TyNat" $
        typeof (TmSucc TmZero) `shouldBe` Right TyNat

      it "typeof (TmPred $ TmSucc TmZero) => TyNat" $
        typeof (TmPred $ TmSucc TmZero) `shouldBe` Right TyNat

      prop "typeof (TmSucc $ [TYPE_ERROR_TERM]) => TypeError" $
        \(t :: WrongTerm) -> do
          typeof (TmSucc $ unWrong t) `shouldBe`
            Left (MismatchWithRequire TyNat TyBool)

      it "typeof (TmIsZero TmZero) => TyBool" $
        typeof (TmIsZero TmZero) `shouldBe` Right TyBool

      it "typeof (TmIsZero TmTrue) => TypeError" $
        typeof (TmIsZero TmTrue) `shouldBe`
          Left (MismatchWithRequire TyNat TyBool)

      it "typeof (TmIf (TmIsZero TmZero) TmTrue TmFalse)" $
        typeof (TmIf (TmIsZero TmZero) TmTrue TmFalse) `shouldBe`
          Right TyBool

      it "typeof (TmIf (TmIsZero TmZero) TmTrue TmFalse)" $
        typeof (TmIf TmTrue (TmSucc TmZero) TmFalse) `shouldBe`
          Left (MultiTypeReturn [("then", TyNat), ("else", TyBool)])

      prop "typof (TmIf [TypeSafe] [TYPE_ERROR_TERM] [TypeSafe])" $
        \(e :: WrongTerm) -> do
          typeof (TmIf TmTrue (unWrong e) TmTrue) `shouldBe`
            Left (MismatchWithRequire TyNat TyBool)

      prop "typof (TmIf [TypeSafe] [TypeSafe] [TYPE_ERROR_TERM])" $
        \(e :: WrongTerm) -> do
          typeof (TmIf TmTrue TmTrue (unWrong e)) `shouldBe`
            Left (MismatchWithRequire TyNat TyBool)
