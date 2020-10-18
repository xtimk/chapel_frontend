module Checker.SymbolTable where
import Data.Map


type SymbolTable = Map String (String, EnvEntry)


data EnvEntry = 
  Variable Loc Type
  | Assignm Loc Type
  | Function Loc [Parameter] Type 
--  | Constant Literal
 deriving (Show)

data Parameter = 
   FunVariable Loc Mode Type
 deriving (Show)

data Mode = Ref | Out | Name | In
 deriving (Show)

data Type = Int | Real | Bool | Void | NotDeclared | Error | ErrorVarNotDeclared Loc String | ErrorIncompatibleTypes Type Type
  deriving (Show)

data Type' = Int' | Real' | Error' ErrorType
  deriving (Show)

data ErrorType = ErrorInitExpr String Loc Type' Type' | ErrorNotInitVar 
  deriving(Show) 

type Loc = (Int,Int)
