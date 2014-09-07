{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Data.Display (Display(..), parens, spaceSep) where

import qualified Data.Text.Lazy.Builder as LB
import qualified Data.Text.Lazy.IO as LIO
import Data.Monoid

class Display a where
  toDisplay :: a -> LB.Builder
  display :: a -> IO ()
  display = LIO.putStrLn . LB.toLazyText . toDisplay

instance Display Char where
  toDisplay = LB.singleton

instance Display String where
  toDisplay = LB.fromString

instance Display LB.Builder where
  toDisplay = id

instance Display Int where
  toDisplay = LB.fromString . show

parens :: (Display a) => a -> LB.Builder
parens d = "(" <> toDisplay d <> ")"

sep :: (Monoid m) => m -> [m] -> m
sep = sep' mempty

sep' :: (Monoid m) => m -> m -> [m] -> m
sep' m _ []     = m
sep' m _ (x:[]) = m <> x
sep' m s (x:xs) = sep' (m <> x <> s) s xs

spaceSep :: (Display a) => [a] -> LB.Builder
spaceSep = sep " " . map toDisplay
