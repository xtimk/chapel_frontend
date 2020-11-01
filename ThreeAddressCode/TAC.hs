module ThreeAddressCode.TAC where

type TAC = [TACEntry]
type Label = Temp

data TACEntry = TACEntry {
    label :: Maybe Label,
    operationType :: TACOperation,
    position :: Loc
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

data Temp = Temp String Loc
    deriving (Show)
data Bop = Bop TacType BopType 
    deriving (Show)
data Uop = Uoq TacType UopType
    deriving (Show)
data Rel = Rel TacType RelType
    deriving (Show)

data TacType = Int | Float | Char | Bool
    deriving (Show)
data UopType = Neg
    deriving (Show) 
data BopType = Plus | Minus | Times | Div 
    deriving (Show)
data RelType = LT | GT | LTE | GTE | EQ | NEQ
    deriving (Show)


type Loc = (Int,Int)
