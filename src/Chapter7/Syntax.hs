{-# LANGUAGE LambdaCase,OverloadedStrings #-}
module Chapter7.Syntax where

import Control.Applicative
import Data.Monoid
import qualified Data.Text.Lazy.Builder as LB

import Data.Display

type Name = String

data Term
  = TmVar Int Int     -- TmVar { index :: Int, contextLength :: Int }
  | TmAbs Name Term
  | TmApp Term Term
  deriving (Eq, Show)

data Binding
  = NameBind
  deriving (Eq, Show)

type Context = [(Name, Binding)]

data RuntimeError
  = WrongContextLength Int Int Int
  | OutOfContextIndex Int
  | UnboundIdentifier Name
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------
(<+) :: Int -> Int -> Term
i <+ l = TmVar i l
infix 4 <+

(+>) :: Name -> Term -> Term
n +> t = TmAbs n t
infixr 2 +>

(<+>) :: Term -> Term -> Term
t1 <+> t2 = TmApp t1 t2
infixl 3 <+>

-- Church id ... λx.x (λ.0)
cId :: Term
cId = "x" +> 0 <+ 1

cIdn :: Name -> Term
cIdn n = n +> 0 <+ 1

-------------------------------------------------------------------------------
-- Boolean
-------------------------------------------------------------------------------

-- Church true ... λt.λf.t (λ.λ.1)
cTru :: Term
cTru = "t" +> "f" +> 1 <+ 2

-- Church false ... λt.λf.f (λ.λ.0)
cFls :: Term
cFls = "t" +> "f" +> 0 <+ 2

-- Church if ... λl.λm.λn.l m n (λ.λ.λ.2 0 1)
cTest :: Term
cTest = "l" +> "m" +> "n" +> (2 <+ 3) <+> (1 <+ 3) <+> (0 <+ 3)

-- Church and ... λb.λc.b c fls (λ.λ.1 0 2↑fls)
cAnd :: Term
cAnd = "b" +> "c" +> (1 <+ 2) <+> (0 <+ 2) <+> 2 -^ cFls

-- Church or  ... λb.λc.b c tru (λ.λ.1 0 ２↑tru)
cOr :: Term
cOr = "b" +> "c" +> (1 <+ 2) <+> 2 -^ cTru <+> (0 <+ 2)

-------------------------------------------------------------------------------
-- Pair
-------------------------------------------------------------------------------

-- Church pair ... λf.λs.λb.b f s (λ.λ.λ.0 2 1)
cPair :: Term
cPair = "f" +> "s" +> "b" +> (0 <+ 3) <+> (2 <+ 3) <+> (1 <+ 3)

-- Church fst  ... λp.p tru (λ.0 1↑tru)
cFst :: Term
cFst = "p" +> (0 <+ 1) <+> 1 -^ cTru

-- Church snd  ... λp.p fls (λ.0 1↑fls)
cSnd :: Term
cSnd = "p" +> (0 <+ 1) <+> 1 -^ cFls

-------------------------------------------------------------------------------
-- Number
-------------------------------------------------------------------------------
-- Church zero ... λs.λz.z (λ.λ.0)
cZro :: Term
cZro = "s" +> "z" +> (0 <+ 2)

-- Church zero ... λs.λz.s z (λ.λ.1 0)
cOne :: Term
cOne = "s" +> "z" +> (1 <+ 2) <+> (0 <+ 2)

-- Church succ ... λn.λs.λz.s (n s z) (λ.λ.λ.1 (2 1 0))
cScc :: Term
cScc = "n" +> "s" +> "z" +> (1 <+ 3) <+> ((2 <+ 3) <+> (1 <+ 3) <+> (0 <+ 3))

-- Church isZero ... λm.m (λx.fls) tru (λ.0 (λ.2↑fls) 1↑tru)
cIsZro :: Term
cIsZro = "m" +> (0 <+ 1) <+> ("x" +> 2 -^ cFls) <+> 1 -^ cTru

-------------------------------------------------------------------------------
-- Display Term & Error
-------------------------------------------------------------------------------

instance Display RuntimeError where
  toDisplay = \case

    WrongContextLength vi vl cl ->
      let [i,l,s] = map toDisplay [vi,vl,cl] in
      "[BAD_INDEX] Value has wrong index " <>
      "(index: " <> i <> ", length: " <> l <> ")\n" <>
      "[INFO] Context has (length: " <> s <> ")"

    OutOfContextIndex i ->
      "[NOT_FOUND] Not found variable " <>
      "(index: " <> toDisplay i <> ") in this context"

    UnboundIdentifier n ->
      "[UNBOUND_ID] Identifier \"" <> toDisplay n <> "\" is unbound"

withContext :: Context -> Term -> LB.Builder
withContext c = \case

  TmAbs n t ->
    let (c', n') = pickFreshName c n in
    parens $ spaceSep $ ["\\", toDisplay n', disp c' t]

  TmApp t1 t2 ->
    parens $ spaceSep $ map (disp c) [t1, t2]

  TmVar i l ->
    eitherDisplay $ indexToName c i l

  where
  disp :: Context -> Term -> LB.Builder
  disp x y = toDisplay $ withContext x y

  eitherDisplay :: (Display a, Display b) => Either a b -> LB.Builder
  eitherDisplay = either toDisplay toDisplay

-------------------------------------------------------------------------------
-- Term Control
-------------------------------------------------------------------------------

pickFreshName :: Context -> Name -> (Context, Name)
pickFreshName c n
  | isNameBound c n = pickFreshName c (n ++ "'")
  | otherwise       = (((n, NameBind) : c), n)

isNameBound :: Context -> Name -> Bool
isNameBound c n = case c of
  [] -> False
  (y:ys)
    | fst y == n -> True
    | otherwise  -> isNameBound ys n

indexToName :: Context -> Int -> Int -> Either RuntimeError Name
indexToName c i l
  | length c /= l = Left $ WrongContextLength i l $ length c
  | length c <= i = Left $ OutOfContextIndex i
  | otherwise     = Right $ fst $ c !! i

nameToIndex :: Context -> Name -> Either RuntimeError Int
nameToIndex c n = case c of
  [] -> Left $ UnboundIdentifier n
  (y:ys)
    | fst y == n -> Right 0
    | otherwise  -> (1 +) <$> nameToIndex ys n

shift :: Int -> Term -> Term
shift d = walk 0
  where
  walk :: Int -> Term -> Term
  walk c = \case
    TmAbs x t   -> x +> walk (c + 1) t
    TmApp t1 t2 -> walk c t1 <+> walk c t2
    TmVar i l
      | i >= l    -> i + d <+ l + d
      | otherwise -> i <+ l + d

(-^) :: Int -> Term -> Term
d -^ t = shift d t
infix 9 -^

subst :: Int -> Term -> Term -> Term
subst j s = walk 0
  where
  walk :: Int -> Term -> Term
  walk c = \case
    TmAbs x t   -> x +> walk (c + 1) t
    TmApp t1 t2 -> walk c t1 <+> walk c t2
    TmVar i l
      | i == j + c -> c -^ s
      | otherwise  -> i <+ l

substTop :: Term -> Term -> Term
substTop s =
  ((-1) -^) . subst 0 (1 -^ s)
