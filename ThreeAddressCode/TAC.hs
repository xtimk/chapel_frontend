module ThreeAddressCode.TAC where

import Utils.AbsUtils
import Utils.Type

type TAC = [TACEntry]
type Label = (String, Loc)

data TACEntry = TACEntry {
    label :: Maybe Label,
    operationType :: TACOperation
} deriving (Show)

data TACOperation =
    Binary Temp Temp Bop Temp |
    Unary Temp Uop Temp |
    Nullary Temp Temp|
    UnconJump Label |
    BoolCondJump Temp Label |
    RelCondJump Temp Rel Temp Label |
    IndexLeft Temp Temp Temp |
    IndexRight Temp Temp Temp |
    DeferenceRight Temp Temp |
    ReferenceLeft Temp Temp |
    ReferenceRight Temp Temp |
    SetParam Temp |
    CallProc Temp Int |
    CallFun Temp Temp Int |
    ReturnVoid |
    ReturnValue Temp |
    VoidOp 
    deriving (Show)

data Temp = Temp TempMode String Loc Type
    deriving (Show)
data Bop =  Plus | Minus | Times | Div | Modul
    deriving (Show)
data Uop = Neg
    deriving (Show)
data Rel =  LT | GT | LTE | GTE | EQ | NEQ | AND | OR
    deriving (Show)

data TempMode = Fix | Var
    deriving (Show)
    
data TacChecker a = TacChecker {
  tacEntries :: [TACEntry],
  datas :: a
} deriving (Show)

data IfSimpleLabels = IfSimpleLabels {
    labelTrue :: Maybe Label,
    labelFalse :: Maybe Label,
    breakLabel :: Maybe Label
} deriving (Show)

data WhileDoLabels = WhileDoLabels {
    labelWhileDoGuardTrue :: Maybe Label,
    labelWhileDoGuardFalse :: Maybe Label,
    labelWhileDoBegin :: Maybe Label
} deriving (Show)

data SequenceControlLabel = 
    SequenceIfSimple IfSimpleLabels |
    SequenceWhileDo WhileDoLabels
        deriving (Show)

addLabelToEntry :: Maybe Label -> TACEntry -> TACEntry
addLabelToEntry label (TACEntry _  operationType) = TACEntry label operationType

sizeof ty = case ty of
    Array ty bound -> sizeof ty
    Int -> 4
    Real -> 8 
    Char -> 1
    Bool -> 1