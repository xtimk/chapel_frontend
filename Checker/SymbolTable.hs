module Checker.SymbolTable where
import Data.Map
import AbsChapel hiding (Type, Mode)
import Utils.Type
import Utils.AbsUtils

type SymbolTable = Map String (String, EnvEntry)

data EnvEntry = 
  Variable Mode Loc Type
  | Assignm Loc Type
  | Function Loc [[EnvEntry]] Type 
--  | Constant Literal
 deriving (Show)



