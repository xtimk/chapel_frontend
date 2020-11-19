module Checker.SymbolTable where
import Data.Map
import Utils.Type
import Utils.AbsUtils

type SymbolTable = Map String (String, EnvEntry)

data EnvEntry = 
  Variable Loc Type Bool
  | Function [(Loc,[EnvEntry])] Type 
 deriving (Show)



