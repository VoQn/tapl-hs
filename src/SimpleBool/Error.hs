module SimpleBool.Error where

import Control.Monad.Error hiding (Error)
import qualified Control.Monad.Error as M

import Data.Info
import SimpleBool.Type

type Message = String

data Error
  = WrongBinding   Info Name      -- ^ wrong kind of binding for variable
  | OutOfContext   Info Int  Int  -- ^ wrong index of context
  | NotFoundNamed  Info Name      -- ^ not found name binding variable
  | MismatchType   Info Type Type -- ^ mismatch types
  | IsNotArrow     Info Type      -- ^ expected arrow type, but recieved others
  | DifferentType  Info Type Type -- ^ include multiple types in expression
  | SomethingWrong Message
  deriving (Eq, Show)

instance M.Error Error where
  noMsg  = SomethingWrong "Something wrong"
  strMsg = SomethingWrong
