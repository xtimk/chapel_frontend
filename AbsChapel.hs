-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module AbsChapel where

newtype POpenGraph = POpenGraph ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PCloseGraph = PCloseGraph ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype POpenParenthesis = POpenParenthesis ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PCloseParenthesis = PCloseParenthesis ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype POpenBracket = POpenBracket ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PCloseBracket = PCloseBracket ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PSemicolon = PSemicolon ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PColon = PColon ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PPoint = PPoint ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PIf = PIf ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PThen = PThen ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PElse = PElse ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Pdo = Pdo ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PWhile = PWhile ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PIntType = PIntType ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PRealType = PRealType ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PCharType = PCharType ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PBoolType = PBoolType ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PStringType = PStringType ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PBreak = PBreak ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PContinue = PContinue ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PAssignmEq = PAssignmEq ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PRef = PRef ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PVar = PVar ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PConst = PConst ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PProc = PProc ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PReturn = PReturn ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PTrue = PTrue ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PFalse = PFalse ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PElthen = PElthen ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PEgrthen = PEgrthen ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PEplus = PEplus ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PEminus = PEminus ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PEtimes = PEtimes ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PEdiv = PEdiv ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PEmod = PEmod ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PDef = PDef ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PElor = PElor ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PEland = PEland ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PEeq = PEeq ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PEneq = PEneq ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PEle = PEle ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PEge = PEge ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PAssignmPlus = PAssignmPlus ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PIdent = PIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PString = PString ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PChar = PChar ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PDouble = PDouble ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PInteger = PInteger ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

data Program = Progr Module
  deriving (Eq, Ord, Show, Read)

data Module = Mod [Ext]
  deriving (Eq, Ord, Show, Read)

data Ext = ExtDecl Declaration | ExtFun Function
  deriving (Eq, Ord, Show, Read)

data Declaration = Decl DecMode [DeclList] PSemicolon
  deriving (Eq, Ord, Show, Read)

data DeclList
    = NoAssgmDec [PIdent] PColon Type
    | NoAssgmArrayFixDec [PIdent] PColon ArDecl
    | NoAssgmArrayDec [PIdent] PColon ArDecl Type
    | AssgmTypeDec [PIdent] PColon Type AssgnmOp ExprDecl
    | AssgmArrayTypeDec [PIdent] PColon ArDecl Type AssgnmOp ExprDecl
    | AssgmArrayDec [PIdent] PColon ArDecl AssgnmOp ExprDecl
    | AssgmDec [PIdent] AssgnmOp ExprDecl
  deriving (Eq, Ord, Show, Read)

data ExprDecl = ExprDecArray ArInit | ExprDec Exp
  deriving (Eq, Ord, Show, Read)

data ArInit = ArrayInit POpenBracket [ExprDecl] PCloseBracket
  deriving (Eq, Ord, Show, Read)

data ArDecl = ArrayDeclIndex POpenBracket [ArDim] PCloseBracket
  deriving (Eq, Ord, Show, Read)

data ArDim
    = ArrayDimSingle ArBound PPoint PPoint ArBound
    | ArrayDimBound ArBound
  deriving (Eq, Ord, Show, Read)

data ArBound = ArrayBoundIdent PIdent | ArratBoundConst Constant
  deriving (Eq, Ord, Show, Read)

data DecMode = PVarMode PVar | PConstMode PConst
  deriving (Eq, Ord, Show, Read)

data Function = FunDec PProc Signature Body
  deriving (Eq, Ord, Show, Read)

data Signature
    = SignNoRet PIdent FunctionParams
    | SignWRet PIdent FunctionParams PColon Type
  deriving (Eq, Ord, Show, Read)

data FunctionParams
    = FunParams POpenParenthesis [Param] PCloseParenthesis
  deriving (Eq, Ord, Show, Read)

data Param
    = ParNoMode [PIdent] PColon Type
    | ParWMode Mode [PIdent] PColon Type
  deriving (Eq, Ord, Show, Read)

data PassedParam = PassedPar Exp
  deriving (Eq, Ord, Show, Read)

data Body = BodyBlock POpenGraph [BodyStatement] PCloseGraph
  deriving (Eq, Ord, Show, Read)

data BodyStatement
    = Stm Statement
    | Fun Function PSemicolon
    | DeclStm Declaration
    | Block Body
  deriving (Eq, Ord, Show, Read)

data Statement
    = DoWhile Pdo PWhile Body Guard
    | While PWhile Guard Body
    | If PIf Guard PThen Body
    | IfElse PIf Guard PThen Body PElse Body
    | RetVal PReturn Exp PSemicolon
    | RetVoid PReturn PSemicolon
    | Continue PContinue PSemicolon
    | Break PBreak PSemicolon
    | StExp Exp PSemicolon
  deriving (Eq, Ord, Show, Read)

data Guard = SGuard POpenParenthesis Exp PCloseParenthesis
  deriving (Eq, Ord, Show, Read)

data Type
    = Tint PIntType
    | Treal PRealType
    | Tchar PCharType
    | Tstring PStringType
    | Tbool PBoolType
    | TPointer PEtimes Type
  deriving (Eq, Ord, Show, Read)

data AssgnmOp = AssgnEq PAssignmEq | AssgnPlEq PAssignmPlus
  deriving (Eq, Ord, Show, Read)

data Mode = RefMode PRef
  deriving (Eq, Ord, Show, Read)

data Exp
    = EAss Exp AssgnmOp Exp
    | Elor Exp PElor Exp
    | Eland Exp PEland Exp
    | Ebitand Exp PDef Exp
    | Eeq Exp PEeq Exp
    | Eneq Exp PEneq Exp
    | Elthen Exp PElthen Exp
    | Egrthen Exp PEgrthen Exp
    | Ele Exp PEle Exp
    | Ege Exp PEge Exp
    | Eplus Exp PEplus Exp
    | Eminus Exp PEminus Exp
    | Etimes Exp PEtimes Exp
    | Ediv Exp PEdiv Exp
    | Emod Exp PEmod Exp
    | Epreop UnaryOperator Exp
    | Earray Exp ArInit
    | InnerExp POpenParenthesis Exp PCloseParenthesis
    | EFun PIdent POpenParenthesis [PassedParam] PCloseParenthesis
    | Evar PIdent
    | Econst Constant
  deriving (Eq, Ord, Show, Read)

data UnaryOperator = Address PDef | Indirection PEtimes
  deriving (Eq, Ord, Show, Read)

data Constant
    = Estring PString
    | Efloat PDouble
    | Echar PChar
    | Eint PInteger
    | ETrue PTrue
    | EFalse PFalse
  deriving (Eq, Ord, Show, Read)

