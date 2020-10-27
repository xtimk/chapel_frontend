module Checker.ErrorPrettyPrinter where

import Checker.SymbolTable
printErrors ::(Foldable t0) => t0 ErrorChecker -> IO ()
printErrors = mapM_ printError 

printError (ErrorChecker (l,c) error) = 
    putStrLn $ "Error in line " ++ show l ++ " and column " ++ show c ++ ": " ++ printDefinedError error

printDefinedError error = case error of
  ErrorVarNotDeclared id -> "Variable " ++ show id ++  " not declared."
  ErrorIncompatibleTypes ty1 ty2 -> "Not compatible types " ++ show ty1 ++  " and "  ++ show ty2 ++ "."
  ErrorIncompatibleDeclTypes id ty1 ty2 -> "Variable " ++ show id ++ " of type " ++ show ty1 ++  " not compatible with type "  ++ show ty2 ++ "."
  ErrorVarAlreadyDeclared (l,c) id -> "Variable " ++ show id ++" already declared in line " ++ show l ++ " and column " ++ show c ++ "."
  ErrorGuardNotBoolean -> "Guard must be boolean."
  ErrorDeclarationBoundNotCorrectType ty id -> "Variable " ++ show id ++ " is of type " ++ show ty ++ " instead of type array."
  ErrorArrayCallExpression -> "Must be call array reference []."
  ErrorDeclarationBoundArray ty const -> "Bound of array must be an Int but was found " ++ show  const ++ " of type " ++ show ty ++ "."
  ErrorWrongDimensionArray arDim callDim id -> "Array variable " ++ show id ++ " is declared of " ++ show arDim ++ " dimensions but is called to keep " ++ show callDim ++ " dimensions."
  ErrorArrayExpressionRequest -> "Must be call array reference []."
  ErrorCantOpToAddress ty -> "Operation with pointer must be with Int or Pointer but was found type "++ show ty ++ "."
  ErrorCantAddressAnExpression -> "An expression cannot address"
  ErrorReturnNotInsideAProcedure -> "Return can be write only in a procedure"
  ErrorCalledProcWithWrongTypeParam pos ty1 ty2 id-> "Parameter in position " ++ show pos ++ " must be of type " ++ show ty1 ++ " but was found type "++ show ty2 ++ " on call function " ++ id ++ "."
  ErrorCalledProcWrongArgs dim1 dim2 id-> "Function " ++ show id ++ " expect " ++ show dim2 ++ " arguments but found " ++ show dim1 ++ " arguments."
  ErrorCalledProcWithVariable id -> "Variable " ++ show id ++ " is not a procedure."
