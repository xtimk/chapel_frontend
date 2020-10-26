module Checker.SymbolTable where
import Data.Map


type SymbolTable = Map String (String, EnvEntry)


data EnvEntry = 
  Variable Mode Loc Type
  | Assignm Loc Type
  | Function Loc [EnvEntry] Type 
--  | Constant Literal
 deriving (Show)

data BlkType = 
  DoWhileBlk |
  WhileBlk |
  ProcedureBlk |
  ExternalBlk |
  SimpleBlk |
  IfSimpleBlk |
  IfThenBlk |
  IfElseBlk
  deriving (Show)

data Mode = Normal | Ref | Out | Name | In 
 deriving (Show)

data DataChecker a = DataChecker {
  datas :: a,
  checkerErrors :: [ErrorChecker]
} deriving (Show)


data Type = Int | Real | Bool | Void | Char | Infered | Array Type (Bound , Bound) | Pointer Type | Error (Maybe Type)
  deriving (Show)

data ErrorChecker =  ErrorChecker Loc DefinedError
 deriving (Show)

data DefinedError = 
  ErrorVarNotDeclared String | 
  ErrorIncompatibleTypes Type Type | 
  ErrorVarAlreadyDeclared Loc String |
  ErrorGuardNotBoolean |
  ErrorDeclarationBoundNotCorrectType Type String |
  ErrorArrayCallExpression |
  ErrorArrayIdentifierType Type String |
  ErrorDeclarationBoundArray Type String |
  ErrorDimensionArray Int Loc Int | 
  ErrorWrongDimensionArray Int Int String |
  ErrorArrayExpressionRequest |
  ErrorCantAddRealToAddress  |
  ErrorCantAddCharToAddress  |
  ErrorCantAddressAnExpression |
  ErrorReturnNotInsideAProcedure |
  ErrorCalledProcWithWrongTypeParam Type Type |
  ErrorCalledProcWithLessArgs |
  ErrorCalledProcWithTooMuchArgs
  deriving (Show)

-- data Type' = Int' | Real' | Error' ErrorType
--   deriving (Show)

-- data ErrorType = ErrorInitExpr String Loc Type' Type' | ErrorNotInitVar 
--   deriving(Show) 

type Loc = (Int,Int)

data Bound = Var String | Fix Int
  deriving (Show)
