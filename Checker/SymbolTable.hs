module Checker.SymbolTable where
import Data.Map


type SymbolTable = Map String (String, EnvEntry)


data EnvEntry = 
  Variable Mode Loc Type
  | Assignm Loc Type
  | Function Loc [EnvEntry] Type 
--  | Constant Literal
 deriving (Show)

data Mode = Normal | Ref | Out | Name | In 
 deriving (Show)

data Type = Int | Real | Bool | Void | Infered | Array ModeType (Bound , Bound) | TypeError [ErrorChecker]
  deriving (Show)

data ModeType = ModeType Mode Type | Error [ErrorChecker]
  deriving (Show)

data ErrorChecker = 
  ErrorVarNotDeclared Loc String | 
  ErrorIncompatibleTypes Loc Type Type | 
  ErrorVarAlreadyDeclared Loc Loc String |
  ErrorGuardNotBoolean Loc |
  ErrorDeclarationBoundNotCorrectType Loc Type String |
  ErrorDeclarationBoundArray Loc Type String |
  ErrorDimensionArray Loc Int Loc Int 
  deriving (Show)

-- data Type' = Int' | Real' | Error' ErrorType
--   deriving (Show)

-- data ErrorType = ErrorInitExpr String Loc Type' Type' | ErrorNotInitVar 
--   deriving(Show) 

type Loc = (Int,Int)

data Bound = Var String | Fix Int
  deriving (Show)
