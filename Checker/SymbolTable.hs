module Checker.SymbolTable where
import Data.Map


type SymbolTable = Map (Int, Int) (String, EnvEntry)


data EnvEntry = 
  Variable Loc Type
  | Assignm Loc Type
--  | Function Loc [Parameter] Type 
--  | Constant Literal
 deriving (Show)

data Type = Int | Real | Bool | Error
  deriving (Show)

data Type' = Int' | Real' | Error' ErrorType
  deriving (Show)

data ErrorType = ErrorInitExpr String Loc Type' Type' | ErrorNotInitVar 
  deriving(Show) 

type Loc = (Int,Int)
