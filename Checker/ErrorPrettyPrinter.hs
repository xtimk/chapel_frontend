module Checker.ErrorPrettyPrinter where
import LexChapel
import Utils.AbsUtils
import Utils.Type
import Checker.SymbolTable
import AbsChapel hiding (Type, Mode)

data DataChecker a = DataChecker {
  datas :: a,
  checkerErrors :: [ErrorChecker]
} deriving (Show)

data DefinedError = 
  ErrorVarNotDeclared String | 
  ErrorIncompatibleTypeArrayIndex Type Exp | 
  ErrorIncompatibleAssgnOpType Loc Type Exp |
  ErrorIncompatibleDeclarationArrayType Type Type ExprDecl  |
  ErrorIncompatibleDeclarationType String Type Type (Maybe ExprDecl) |
  ErrorIncompatibleReturnType String Type Type (Maybe Exp) |
  ErrorIncompatibleUnaryOpType Loc Type Type Exp |
  ErrorIncompatibleBinaryOpType Loc Type Type Exp Exp |
  ErrorMissingReturn String | 
  ErrorVarAlreadyDeclared Loc String |
  ErrorSignatureAlreadyDeclared  (Loc,Loc)  Loc String |
  ErrorGuardNotBoolean Exp Type|
  ErrorDeclarationCallWithZero |
  ErrorDeclarationBoundOnlyConst String |
  ErrorDeclarationBoundNotCorrectType Type String |
  ErrorArrayCallExpression Exp|
  ErrorArrayIdentifierType Type String |
  ErrorDeclarationBoundArray Type String |
  ErrorBoundsArray Int Int |
  ErrorDimensionArray Int Loc Int | 
  ErrorWrongDimensionArray Int Int String |
  ErrorArrayExpressionRequest ExprDecl|
  ErrorCantAddressAnExpression Exp|
  ErrorReturnNotInsideAProcedure |
  ErrorInterruptNotInsideAProcedure String |
  ErrorCalledProcWithWrongTypeParam Int Type Mode Type Mode String|
  ErrorCalledProcWrongArgs Int Int String |
  ErrorCalledProcWithVariable String |
  ErrorNoPointerAddress Type Exp|
  ErrorAssignDecl String|
  ErrorOnlyRightExpression Exp |
  ErrorOnlyLeftExpression Exp |
  ErrorNotLeftExpression Exp AssgnmOp |
  ErrorOverloadingIncompatibleReturnType Loc Loc String Type Type |
  NoDecucibleType String |
  ErrorFunctionWithNotEnoughReturns String |
  ErrorCantUseExprInARefPassedVar Exp | 
  ErrorCyclicDeclaration Loc String |
  ErrorMissingInitialization String |
  ErrorForEachIteratorArray Type Exp |
  ErrorForEachItem Exp 
  deriving (Show)

data ErrorChecker =  ErrorChecker Loc DefinedError
 deriving (Show)

instance Eq ErrorChecker where
  (ErrorChecker (l1,c1) _)  == (ErrorChecker (l2,c2) _) = l1 == l2 && c1 == c2

instance Ord ErrorChecker where
  (ErrorChecker (l1,c1) _)  > (ErrorChecker (l2,c2) _) 
    | l1 == l2 && c1 > c2 = True
    | l1 > l2 = True
    | otherwise = False
  x1 <= x2 = not $ x1 > x2

printErrors ::(Foldable t0) => [Token] -> t0 ErrorChecker -> IO ()
printErrors tokens = mapM_ (printError tokens)

printError tokens (ErrorChecker (l,c) error) = 
    putStrLn $ "Error in line " ++ show l ++ " and column " ++ show c ++ ": " ++ printDefinedError tokens error

printDefinedError tokens error = case error of
  ErrorVarNotDeclared id -> "Variable " ++ id ++ " not declared."
  ErrorIncompatibleTypeArrayIndex tyFound exp -> "Only type Int is accepted for indexing an array but was found type " ++ prettyPrinterType tyFound ++ " in expression " ++ printExpression tokens exp
  ErrorIncompatibleAssgnOpType locOp ty exp -> "Assignment operation " ++ printTokensRange tokens (locOp,locOp) ++ " is not compatible with type " ++ prettyPrinterType ty ++ " for left expression " ++ printExpression tokens exp
  ErrorIncompatibleDeclarationArrayType tyAr tyFound exp -> "On actual array declaration cell type found expect is " ++ prettyPrinterType tyAr ++ " but was found type " ++ prettyPrinterType tyFound ++ " in expression " ++ printDeclExpression tokens exp ++ "."
  ErrorIncompatibleDeclarationType idVar ty tyexp exp -> case exp of
    Just exp -> "Variable \"" ++ idVar ++ "\" is of type " ++ prettyPrinterType ty ++ " but was found type " ++ prettyPrinterType tyexp ++ " in expression " ++ printDeclExpression tokens exp ++ "."
    Nothing -> "non lo raggiunge"
  ErrorIncompatibleReturnType idFun tyFun tyExp exp -> case exp of
    Just exp -> "Function \"" ++ idFun ++ "\" returns type " ++ prettyPrinterType tyFun ++  ", but expression " ++ printExpression tokens exp   ++ " in return statement is of type "  ++ prettyPrinterType tyExp ++ "." 
    Nothing -> "Function \"" ++ idFun ++ "\" returns type " ++ prettyPrinterType tyFun ++  " but nothing is returned " 
  ErrorIncompatibleUnaryOpType locOp ty1 ty2 e -> "Operation " ++ printTokensRange tokens (locOp,locOp) ++ " requires type " ++ prettyPrinterType ty1 ++ " but was found type " ++ prettyPrinterType ty2 ++ " in expression " ++ printExpression tokens e ++ "."
  ErrorIncompatibleBinaryOpType locOp ty1 ty2 e1 e2 -> "Not compatible types " ++ prettyPrinterType ty1 ++  " and "  ++ prettyPrinterType ty2 ++ " for operation " ++ printTokensRange tokens (locOp,locOp) ++ " between expressions " ++ printExpression tokens e1 ++ " and " ++ printExpression tokens e2 ++ "." 
  ErrorVarAlreadyDeclared (l,c) id -> "Variable " ++ id ++" already declared in line " ++ show l ++ " and column " ++ show c ++ "."
  ErrorGuardNotBoolean exp ty -> "Guard must be boolean but type " ++ prettyPrinterType ty ++ " was found in expression " ++ printExpression tokens exp
  ErrorDeclarationBoundNotCorrectType ty id -> "Variable " ++ id ++ " is of type " ++ prettyPrinterType ty ++ " instead of type array."
  ErrorDeclarationCallWithZero-> "Bound can be only a constant greater than 0"
  ErrorDeclarationBoundOnlyConst id -> "Bound can be only a constant but variable " ++ id ++ " was found."
  ErrorArrayCallExpression exp -> "Only a variable can index like an array but was found expression " ++ printExpression tokens exp ++ "."
  ErrorDeclarationBoundArray ty const -> "Bound of array must be an Int but was found " ++ show const ++ " of type " ++ prettyPrinterType ty ++ "."
  ErrorBoundsArray boundLeft boundRight -> "In arrays bound right must be greater than bound left but was found " ++ show boundLeft ++ " in bound left and " ++ show boundRight ++ " in bound right."
  ErrorWrongDimensionArray arDim callDim id -> "Array variable " ++ id ++ " is declared of " ++ show arDim ++ " dimensions but is called to keep " ++ show callDim ++ " dimensions."
  ErrorArrayExpressionRequest expDecl-> "Must be call an array indexing like \" a[2,3] \" but was found an array declaration " ++ printDeclExpression tokens expDecl ++ "."
  ErrorCantAddressAnExpression exp -> "An expression cannot address in " ++ printExpression tokens exp ++ "."
  ErrorReturnNotInsideAProcedure -> "Return can be write only in a procedure"
  ErrorCalledProcWithWrongTypeParam pos ty1 mode1 ty2 mode2 id-> "On call function \"" ++ id ++ "\" parameter in position " ++ show pos ++ " must be of type " ++ prettyPrinterType ty1  ++ prettyPrinterMode mode1 ++ ", " ++ 
    "but was found type " ++ prettyPrinterType ty2 ++ prettyPrinterMode mode2 ++ "."
  ErrorCalledProcWrongArgs dim1 dim2 id-> "Function " ++ id ++ " expect " ++ show dim1 ++ " arguments but found " ++ show dim2 ++ " arguments."
  ErrorCalledProcWithVariable id -> "Variable " ++ id ++ " is not a procedure."
  ErrorNoPointerAddress ty exp -> "Can only addressed a pointer but was found expression " ++ printExpression tokens exp ++ " of type " ++ prettyPrinterType ty ++ "."
  ErrorAssignDecl id -> "Cannot make implicit operation on declaration in variable \"" ++ id ++ "\"."
  ErrorNotLeftExpression exp assgn  -> let expPos = getStartExpPos exp; (l,c) = getAssignPos assgn in  
    "Required left expression before the assignment but was found "  ++ printTokens (getTokens tokens expPos (l,c - 1)) ++ "."
  ErrorMissingReturn funname -> "In Function " ++ funname ++ ": Specify at least one return."
  ErrorSignatureAlreadyDeclared  locs (l,c) id -> "Signature with name " ++ id ++" and parameters type " ++ printTokensRange tokens locs ++ " already declared in line " ++ show l ++ " and column " ++ show c ++ "."
  ErrorOverloadingIncompatibleReturnType locStart (l,c) id ty1 ty2 -> "Overloading signature for function " ++ id ++ " must be of type " ++ prettyPrinterType ty1 ++ " but was found type " ++ prettyPrinterType ty2 ++ " in " ++ printTokens (getTokens tokens locStart (l,c - 1)) ++ "."
  ErrorInterruptNotInsideAProcedure interuptTy -> interuptTy ++ " command must be inside a while or dowhile."
  ErrorOnlyLeftExpression exp -> "Statement must be an assignment or left expression but was found " ++  printExpression tokens exp ++ "."
  ErrorOnlyRightExpression exp -> "Statement must be a right expression but was found " ++  printExpression tokens exp ++ "."
  NoDecucibleType id -> "Impossible infered type from expression for variable " ++ show id ++ "."
  ErrorFunctionWithNotEnoughReturns funname -> "In function " ++ show funname ++ ": there is a possible path in the code with no returns."
  ErrorCantUseExprInARefPassedVar exp -> "You can't pass an expr by ref, but only a variable. Instead was found " ++  printExpression tokens exp ++ "."
  ErrorCyclicDeclaration (l,c) id -> "Cyclic declaration with variable " ++ id ++ " in line " ++ show l ++ " and column " ++ show c ++ "." 
  ErrorMissingInitialization id -> "Missing initialization for variable " ++ id ++ "."
  ErrorForEachIteratorArray ty exp -> "Iterator must be of type array but was found type " ++ prettyPrinterType ty ++ " in expression " ++ printExpression tokens exp ++ "." 
  ErrorForEachItem exp -> "Item of iteration must be a variable but was found an expression " ++ printExpression tokens exp ++ "."

prettyPrinterMode mode = case mode of
  Ref -> "in reference modality"
  _ -> ""

prettyPrinterType ty  = case ty of
  tyAr@(Array ty _) -> if checkArraywithNotGoodDimension tyAr 
    then "array of dimension [" ++ show (getArrayLenght tyAr) ++ prettyPrinterTypeArray ty
    else "array of dimension not possible to calculate (check for other errors )"
  Pointer ty -> "*" ++ prettyPrinterType ty
  Error -> "impossible to infered from context ( check for other errors )"
  _ -> show ty

checkArraywithNotGoodDimension tyAr@(Array ty _) = let length = getArrayLenght tyAr in length /= (-1) && checkArraywithNotGoodDimension ty
checkArraywithNotGoodDimension _ = True  

prettyPrinterTypeArray tyAr@(Array ty _) = let length = getArrayLenght tyAr in
   "," ++ show length ++ prettyPrinterTypeArray ty
prettyPrinterTypeArray ty = "] of type " ++ prettyPrinterType ty

  

createNewError errorFun isCompatible = [errorFun | not isCompatible]

errorOverloadingReturn loc locStart locEnd id ty1 ty2 = ErrorChecker loc $ ErrorOverloadingIncompatibleReturnType locStart locEnd id ty1 ty2
errorIncompatibleTypeArrayIndex loc ty exp = ErrorChecker loc $ ErrorIncompatibleTypeArrayIndex ty exp
errorIncompatibleAssgnOpType loc ty exp  = ErrorChecker loc $ ErrorIncompatibleAssgnOpType loc ty exp
errorIncompatibleDeclarationArrayType loc ty tyexp exp =  ErrorChecker loc $ ErrorIncompatibleDeclarationArrayType ty tyexp exp
errorIncompatibleDeclarationType loc idVar ty tyexp exp = ErrorChecker loc $ ErrorIncompatibleDeclarationType idVar ty tyexp exp
errorIncompatibleReturnType loc idFun tyFun tyExp exp = ErrorChecker loc $ ErrorIncompatibleReturnType idFun tyFun tyExp exp
errorIncompatibleUnaryType loc e ty1 ty2 = ErrorChecker loc $ ErrorIncompatibleUnaryOpType loc ty1 ty2 e
errorIncompatibleBinaryType loc e1 e2 ty1 ty2  = ErrorChecker loc $ ErrorIncompatibleBinaryOpType loc ty1 ty2 e1 e2
errorIncompatibleTypesChangeToFun loc pos id ty1 mode1 ty2 mode2= ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam pos ty1 mode1 ty2 mode2 id 



printDeclExpression tokens declExpr = printTokens $ getTokens tokens (getExprDeclPos declExpr)  (getEndExprDeclPos declExpr)
printExpression tokens expr = printTokens $ getTokens tokens (getStartExpPos expr)  (getEndExpPos expr)
printTokensRange tokens (locStart, locEnd) = printTokens $ getTokens tokens locStart locEnd

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
  T_PEpow id -> id ++ printTokens' xs
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
  T_PQuestion id -> id ++ printTokens' xs


