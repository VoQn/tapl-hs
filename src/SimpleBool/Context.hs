module SimpleBool.Context where

import Control.Monad.Error hiding (Error)
import qualified Control.Monad.Error as ME

import Data.Info
import qualified Data.Evaluator as EV
import SimpleBool.Type

data Binding
  = NameBind      -- ^ λx.x (identifier binding with name)
  | VarBind  Type -- ^ λx:T (identifier binding with type)
  deriving (Eq, Show)

type Name = String

type Context = [(Name, Binding)]

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

instance ME.Error Error where
  noMsg  = SomethingWrong "Something wrong"
  strMsg = SomethingWrong

type Eval a = EV.Eval Context Error a

class HasType a where
  typeof :: a -> Eval Type

runEval :: Context -> Eval a -> IO (Either Error a, Context)
runEval ctx ev = EV.runEval ctx ev
