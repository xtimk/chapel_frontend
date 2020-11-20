module Checker.SymbolTable where
import Data.Map
import Utils.Type
import Utils.AbsUtils

type SymbolTable = Map String (String, EnvEntry)

data EnvEntry = 
  Variable Loc Type
  | Function [(Loc,[EnvEntry])] Type 
 deriving (Show)



