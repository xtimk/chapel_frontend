module ThreeAddressCode.TAC where

import Utils.AbsUtils
import Utils.Type

type TAC = [TACEntry]
type Label = (String, Loc, LabelType)

data LabelType = 
    TrueBoolStmLb | FalseBoolStmLb | ExitBoolStmLb | 
    DoWhileLb | DoWhileExitLb |
    ForEachLb | ForEachExitLb |
    WhileLb | WhileExitLb |
    IfLb |  IfThenLb | ElseLb | 
    FunLb |
    BreakLb | ContinueLb | 
    VoidLb |
    StringLb
    deriving (Show)

type TACComment = Maybe String

data TACEntry = TACEntry {
    label :: Maybe Label,
    operationType :: TACOperation,
    comment :: TACComment
} deriving (Show)

data TACOperation =
    Binary Temp Temp Bop Temp |
    Unary Temp Uop Temp |
    Nullary Temp Temp|
    UnconJump Label |
    BoolTrueCondJump Temp Label |
    BoolFalseCondJump Temp Label |
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
    VoidOp |
    Cast Temp CastOp Temp |
    StringOp String |
    CommentOp String
    deriving (Show)

data CastOp = CastIntToFloat | CastCharToInt | CastCharToFloat | CastIntToChar
    deriving(Show)

data Temp = Temp TempMode String Loc Type
    deriving (Show)
data Bop =  Plus | Minus | Times | Div | Modul | Pow
    deriving (Show)
data Uop = Neg | MinusUnaryOp
    deriving (Show)
data Rel =  LT | GT | LTE | GTE | EQ | NEQ | AND | OR
    deriving (Show)

data TempMode = Fixed | Variable | Temporary
    deriving (Show)
    
data TacChecker a = TacChecker {
  tacEntries :: [TACEntry],
  datas :: a
} deriving (Show)

data SequenceLazyEvalLabels = SequenceLazyEvalLabels {
    labelTrue :: Label,
    labelFalse :: Label,
    breakLabel :: Label
} deriving (Show)

addLabelToEntry :: Maybe Label -> TACEntry -> TACEntry
addLabelToEntry label (TACEntry _  operationType _comment) = TACEntry label operationType _comment

noComment = Nothing

getSubArrayLength 0 ty = getTotalArrayLength ty
getSubArrayLength depth (Array ty _) =  getSubArrayLength (depth - 1 ) ty

getTotalArrayLength arty@(Array ty _) = getArrayLenght arty * getTotalArrayLength ty
getTotalArrayLength types = sizeof types

sizeof ty = case ty of
    Array ty _ -> sizeof ty
    Pointer ty -> sizeof ty
    Int -> 4
    Real -> 8 
    Char -> 1
    Bool -> 1
    String -> 8


notRel ThreeAddressCode.TAC.LT = GTE
notRel ThreeAddressCode.TAC.LTE= ThreeAddressCode.TAC.GT
notRel ThreeAddressCode.TAC.GT = LTE
notRel ThreeAddressCode.TAC.GTE = ThreeAddressCode.TAC.LT
notRel ThreeAddressCode.TAC.EQ = NEQ
notRel NEQ = ThreeAddressCode.TAC.EQ
-- non dovrei mai arrivare a chiamare i due casi sotto, se ci arrivo c'e qualcosa di sbagliato
-- notRel ThreeAddressCode.TAC.AND = OR
-- notRel ThreeAddressCode.TAC.OR = AND