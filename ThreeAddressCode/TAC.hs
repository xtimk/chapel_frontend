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

data Temp = Temp String Loc TacType
    deriving (Show)
data Bop = Bop TacType BopType 
    deriving (Show)
data Uop = Uop TacType UopType
    deriving (Show)
data Rel = Rel TacType RelType
    deriving (Show)

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

data TacType = Int | Float | Char | Bool | String
    deriving (Show)
data UopType = Neg
    deriving (Show) 
data BopType = Plus | Minus | Times | Div | Modul
    deriving (Show)
data RelType = LT | GT | LTE | GTE | EQ | NEQ
    deriving (Show)

data TacChecker a = TacChecker {
  tacEntries :: [TACEntry],
  datas :: a
} deriving (Show)

type Loc = (Int,Int)
    