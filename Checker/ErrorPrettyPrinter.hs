module Checker.ErrorPrettyPrinter where
import LexChapel
import Utils.AbsUtils
import Utils.Type
import AbsChapel hiding (Type, Mode)

data DataChecker a = DataChecker {
  datas :: a,
  checkerErrors :: [ErrorChecker]
} deriving (Show)

data DefinedError = 
  ErrorVarNotDeclared String | 
  ErrorIncompatibleTypes Type Type | 
  ErrorIncompatibleDeclTypes String Type Type | 
  ErrorMissingReturn String | 
  ErrorIncompatibleRetTypes String Type Type | 
  ErrorVarAlreadyDeclared Loc String |
  ErrorSignatureAlreadyDeclared Loc String |
  ErrorGuardNotBoolean |
  ErrorDeclarationBoundNotCorrectType Type String |
  ErrorArrayCallExpression |
  ErrorArrayIdentifierType Type String |
  ErrorDeclarationBoundArray Type String |
  ErrorDimensionArray Int Loc Int | 
  ErrorWrongDimensionArray Int Int String |
  ErrorArrayExpressionRequest |
  ErrorCantOpToAddress Type|
  ErrorCantAddressAnExpression |
  ErrorReturnNotInsideAProcedure |
  ErrorBreakNotInsideAProcedure |
  ErrorContinueNotInsideAProcedure |
  ErrorCalledProcWithWrongTypeParam Int Type Type String|
  ErrorCalledProcWrongArgs Int Int String |
  ErrorCalledProcWithVariable String |
  ErrorNoPointerAddress Type String |
  ErrorAssignDecl |
  ErrorOnlyRightExpression Exp |
  ErrorNotLeftExpression Exp AssgnmOp |
  ErrorOverloadingIncompatibleReturnType Loc Loc String Type Type |
  ErrorFunctionVoid |
  ErrorReturnNotVoid |
  NoDecucibleType String |
  ErrorFunctionWithNotEnoughReturns String |
  IncompatibleArrayDimension Int Int |
  ErrorCantUseExprInARefPassedVar
  deriving (Show)

data ErrorChecker =  ErrorChecker Loc DefinedError
 deriving (Show)

printErrors ::(Foldable t0) => [Token] -> t0 ErrorChecker -> IO ()
printErrors tokens = mapM_ (printError tokens)

printError tokens (ErrorChecker (l,c) error) = 
    putStrLn $ "Error in line " ++ show l ++ " and column " ++ show c ++ ": " ++ printDefinedError tokens error

printDefinedError tokens error = case error of
  ErrorVarNotDeclared id -> "Variable " ++ id ++  " not declared."
  ErrorIncompatibleTypes ty1 ty2 -> "Not compatible types " ++ show ty1 ++  " and "  ++ show ty2 ++ "."
  ErrorIncompatibleDeclTypes id ty1 ty2 -> "Variable " ++ id ++ " of type " ++ show ty1 ++  " not compatible with type "  ++ show ty2 ++ "."
  ErrorIncompatibleRetTypes id ty1 ty2 -> "Function " ++ id ++ " returns type " ++ show ty1 ++  ", but exp in return statement is "  ++ show ty2 ++ "." 
  ErrorVarAlreadyDeclared (l,c) id -> "Variable " ++ id ++" already declared in line " ++ show l ++ " and column " ++ show c ++ "."
  ErrorGuardNotBoolean -> "Guard must be boolean."
  ErrorDeclarationBoundNotCorrectType ty id -> "Variable " ++ id ++ " is of type " ++ show ty ++ " instead of type array."
  ErrorArrayCallExpression -> "Must be call array reference []."
  ErrorDeclarationBoundArray ty const -> "Bound of array must be an Int but was found " ++ show  const ++ " of type " ++ show ty ++ "."
  ErrorWrongDimensionArray arDim callDim id -> "Array variable " ++ id ++ " is declared of " ++ show arDim ++ " dimensions but is called to keep " ++ show callDim ++ " dimensions."
  ErrorArrayExpressionRequest -> "Must be call array reference []."
  ErrorCantOpToAddress ty -> "Operation with pointer must be with Int or Pointer but was found type "++ show ty ++ "."
  ErrorCantAddressAnExpression -> "An expression cannot address"
  ErrorReturnNotInsideAProcedure -> "Return can be write only in a procedure"
  ErrorCalledProcWithWrongTypeParam pos ty1 ty2 id-> "Parameter in position " ++ show pos ++ " must be of type " ++ show ty1 ++ " but was found type "++ show ty2 ++ " on call function " ++ id ++ "."
  ErrorCalledProcWrongArgs dim1 dim2 id-> "Function " ++ id ++ " expect " ++ show dim1 ++ " arguments but found " ++ show dim2 ++ " arguments."
  ErrorCalledProcWithVariable id -> "Variable " ++ id ++ " is not a procedure."
  ErrorNoPointerAddress ty id -> "Can only addressed a pointer but was found variable " ++ id ++ " of type " ++ show ty ++ "."
  ErrorAssignDecl -> "Cannot make implicit operation on declaration"
  ErrorNotLeftExpression exp assgn  -> let expPos = getExpPos exp; (l,c) = getAssignPos assgn in  
    "Required left expression before the assignment but was found "  ++ printTokens (getTokens tokens expPos (l,c - 1)) ++ "."
  ErrorMissingReturn funname -> "In Function " ++ funname ++ ": Specify at least one return."
  ErrorSignatureAlreadyDeclared (l,c) id -> "Signature " ++ id ++" with this parameter already declared in line " ++ show l ++ " and column " ++ show c ++ "."
  ErrorOverloadingIncompatibleReturnType locStart (l,c) id ty1 ty2 -> "Overloading signature for function " ++ id ++ " must be of type " ++ show ty1 ++ " but was found type " ++ show ty2 ++ " in " ++ printTokens (getTokens tokens locStart (l,c - 1)) ++ "."
  ErrorBreakNotInsideAProcedure -> "Break command must be inside a while or dowhile."
  ErrorContinueNotInsideAProcedure -> "Continue command must be inside a while or dowhile."
  ErrorOnlyRightExpression _exp -> "Statement must be an assignment or left expression"
  ErrorFunctionVoid -> "A procedure cannot return a value"
  ErrorReturnNotVoid -> "Function must return a value"
  NoDecucibleType id -> "Impossible infered type from expression for variable " ++ show id ++ "."
  ErrorFunctionWithNotEnoughReturns funname -> "In function " ++ show funname ++ ": there is a possible path in the code with no returns."
  IncompatibleArrayDimension dim1 dim2 -> "Incompatible array dimension. First is "++ show dim1 ++ " and second is " ++  show dim2 ++"."
  ErrorCantUseExprInARefPassedVar -> "You can't pass an expr by ref, but only a variable."

getTokens [] _ _ = []
getTokens (x@(PT (Pn _ l c ) _ ):xs) start@(lstart, cstart) end@(lend, cend) 
  | l < lstart = getTokens xs start end
  | l == lstart && l < lend && c < cstart =  getTokens xs start end
  | l == lstart && l < lend && c >= cstart =  x:getTokens xs start end
  | l > lstart && l < lend = x:getTokens xs start end
  | l > lstart && l == lend && c <= cend = x:getTokens xs start end
  | l == lstart && l == lend && c >= cstart  && c <= cend = x:getTokens xs start end
  | l == lstart && l == lend && c < cstart = getTokens xs start end
  | otherwise = []

printTokens tokens = let tokenString = printTokens' tokens in case tokenString of
  x:xs -> "\" " ++ xs ++ " \""
  _ -> tokenString

printTokens' [] = ""
printTokens' (x@(PT (Pn _ l c ) token ):xs) = " " ++ case token of
  TS id _-> id ++ printTokens' xs    -- reserved words and symbols
  TL id -> id ++ printTokens' xs         -- string literals
  TI id -> id ++ printTokens' xs         -- integer literals
  TV id -> id ++ printTokens' xs         -- identifiers
  TD id -> id ++ printTokens' xs         -- double precision float literals
  TC id -> id ++ printTokens' xs         -- character literals
  T_POpenGraph id -> id ++ printTokens' xs
  T_PCloseGraph id -> id ++ printTokens' xs
  T_POpenParenthesis id -> id ++ printTokens' xs
  T_PCloseParenthesis id -> id ++ printTokens' xs
  T_POpenBracket id -> id ++ printTokens' xs
  T_PCloseBracket id -> id ++ printTokens' xs
  T_PSemicolon id -> id ++ printTokens' xs
  T_PColon id -> id ++ printTokens' xs
  T_PPoint id -> id ++ printTokens' xs
  T_PIf id -> id ++ printTokens' xs
  T_PThen id -> id ++ printTokens' xs
  T_PElse id -> id ++ printTokens' xs
  T_Pdo id -> id ++ printTokens' xs
  T_PWhile id -> id ++ printTokens' xs
  T_PIntType id -> id ++ printTokens' xs
  T_PRealType id -> id ++ printTokens' xs
  T_PCharType id -> id ++ printTokens' xs
  T_PBoolType id -> id ++ printTokens' xs
  T_PStringType id -> id ++ printTokens' xs
  T_PAssignmEq id -> id ++ printTokens' xs
  T_PRef id -> id ++ printTokens' xs
  T_PVar id -> id ++ printTokens' xs
  T_PNeg id -> id ++ printTokens' xs
  T_PProc id -> id ++ printTokens' xs
  T_PReturn id -> id ++ printTokens' xs
  T_PTrue id -> id ++ printTokens' xs
  T_PFalse id -> id ++ printTokens' xs
  T_PElthen id -> id ++ printTokens' xs
  T_PEgrthen id -> id ++ printTokens' xs
  T_PEplus id -> id ++ printTokens' xs
  T_PEminus id -> id ++ printTokens' xs
  T_PEtimes id -> id ++ printTokens' xs
  T_PEdiv id -> id ++ printTokens' xs
  T_PEmod id -> id ++ printTokens' xs
  T_PDef id -> id ++ printTokens' xs
  T_PElor id -> id ++ printTokens' xs
  T_PEland id -> id ++ printTokens' xs
  T_PEeq id -> id ++ printTokens' xs
  T_PEneq id -> id ++ printTokens' xs
  T_PEle id -> id ++ printTokens' xs
  T_PEge id -> id ++ printTokens' xs
  T_PAssignmPlus id -> id ++ printTokens' xs
  T_PIdent id -> id ++ printTokens' xs
  T_PString id -> id ++ printTokens' xs
  T_PChar id -> id ++ printTokens' xs
  T_PDouble id -> id ++ printTokens' xs
  T_PInteger id -> id ++ printTokens' xs


