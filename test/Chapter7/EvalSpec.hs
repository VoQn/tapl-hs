module Chapter7.EvalSpec where

import Test.Hspec

import Chapter7.Syntax
import Chapter7.Eval

spec :: Spec
spec = do

  describe "isVal" $ do

    it "isVal (\\ x x) => True" $
      isVal [] (TmAbs "x" $ TmVar 0 1) `shouldBe` True

    it "isVal \\.0 => False" $
      isVal [] (TmVar 0 1) `shouldBe` False
