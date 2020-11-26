module Checker.SymbolTable where
import Data.Map
import Utils.Type
import Utils.AbsUtils

type SymbolTable = Map String (String, EnvEntry)


data Mode = Normal | Ref | Out | Name | In 
 deriving (Show)

data EnvEntry = 
  Variable Loc Type Mode
  | Function [(Loc,[EnvEntry])] Type 
 deriving (Show)
