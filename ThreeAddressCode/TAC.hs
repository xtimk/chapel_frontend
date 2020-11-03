module ThreeAddressCode.TAC where

type TAC = [TACEntry]
type Label = Temp

data TACEntry = TACEntry {
    label :: Maybe Label,
    position :: Loc,
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
    RefereceneRight Temp Temp |
    SetParam Temp |
    CallProc Temp Temp |
    CallFun Temp Temp Temp |
    ReturnVoid |
    ReturnValue Temp 
    deriving (Show)

data Temp = Temp TempMode String Loc TacType
    deriving (Show)
data Bop =  Plus | Minus | Times | Div | Modul
    deriving (Show)
data Uop = Neg
    deriving (Show)
data Rel =  LT | GT | LTE | GTE | EQ | NEQ
    deriving (Show)

data TempMode = Fix | Var
    deriving (Show)

data TacType = Int | Float | Char | Bool | String
    deriving (Show)

data TacChecker a = TacChecker {
  tacEntries :: [TACEntry],
  datas :: a
} deriving (Show)

type Loc = (Int,Int)

tacSup Int Int = Int
tacSup Int Float = Float
tacSup Float Int = Float
tacSup Char Int = Int
tacSup Int Char = Int
tacSup String Char = String
tacSup Char String = String
tacSup Char Char = Char
tacSup String String = String
tacSup Float Float = Float
tacSup Bool Bool = Bool

sizeof ty = case ty of
    Int -> 4
    Float -> 8 
    Char -> 1
    Bool -> 1