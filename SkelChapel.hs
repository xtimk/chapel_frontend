module SkelChapel where

-- Haskell module generated by the BNF converter

import AbsChapel
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transPOpenGraph :: POpenGraph -> Result
transPOpenGraph x = case x of
  POpenGraph string -> failure x
transPCloseGraph :: PCloseGraph -> Result
transPCloseGraph x = case x of
  PCloseGraph string -> failure x
transPOpenParenthesis :: POpenParenthesis -> Result
transPOpenParenthesis x = case x of
  POpenParenthesis string -> failure x
transPCloseParenthesis :: PCloseParenthesis -> Result
transPCloseParenthesis x = case x of
  PCloseParenthesis string -> failure x
transPSemicolon :: PSemicolon -> Result
transPSemicolon x = case x of
  PSemicolon string -> failure x
transPIdent :: PIdent -> Result
transPIdent x = case x of
  PIdent string -> failure x
transPString :: PString -> Result
transPString x = case x of
  PString string -> failure x
transPChar :: PChar -> Result
transPChar x = case x of
  PChar string -> failure x
transPDouble :: PDouble -> Result
transPDouble x = case x of
  PDouble string -> failure x
transPInteger :: PInteger -> Result
transPInteger x = case x of
  PInteger string -> failure x
transPAssignmEq :: PAssignmEq -> Result
transPAssignmEq x = case x of
  PAssignmEq string -> failure x
transPAssignmPlus :: PAssignmPlus -> Result
transPAssignmPlus x = case x of
  PAssignmPlus string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Progr module_ -> failure x
transModule :: Module -> Result
transModule x = case x of
  Mod exts -> failure x
transExt :: Ext -> Result
transExt x = case x of
  ExtDecl declaration -> failure x
  ExtFun function -> failure x
transDeclaration :: Declaration -> Result
transDeclaration x = case x of
  NoAssgmDec type_ pident psemicolon -> failure x
  AssgmDec type_ pident assgnmop exp psemicolon -> failure x
transFunction :: Function -> Result
transFunction x = case x of
  FunDec signature body -> failure x
transSignature :: Signature -> Result
transSignature x = case x of
  Sign type_ pident functionparams -> failure x
transFunctionParams :: FunctionParams -> Result
transFunctionParams x = case x of
  FunParams popenparenthesis params pcloseparenthesis -> failure x
transParam :: Param -> Result
transParam x = case x of
  ParNoMode type_ pident -> failure x
  ParWMode mode type_ pident -> failure x
transBody :: Body -> Result
transBody x = case x of
  FunBlock popengraph bodystatements pclosegraph -> failure x
transBodyStatement :: BodyStatement -> Result
transBodyStatement x = case x of
  Stm statement -> failure x
  Fun function psemicolon -> failure x
  Decl declaration -> failure x
  Block body -> failure x
transStatement :: Statement -> Result
transStatement x = case x of
  DoWhile body guard -> failure x
  StExp exp psemicolon -> failure x
transGuard :: Guard -> Result
transGuard x = case x of
  SGuard popenparenthesis exp pcloseparenthesis -> failure x
transType :: Type -> Result
transType x = case x of
  Tint -> failure x
transAssgnmOp :: AssgnmOp -> Result
transAssgnmOp x = case x of
  AssgnEq passignmeq -> failure x
  AssgnPlEq passignmplus -> failure x
transMode :: Mode -> Result
transMode x = case x of
  RefMode -> failure x
transExp :: Exp -> Result
transExp x = case x of
  EAss exp1 assgnmop exp2 -> failure x
  Elor exp1 exp2 -> failure x
  Eland exp1 exp2 -> failure x
  Ebitor exp1 exp2 -> failure x
  Ebitexor exp1 exp2 -> failure x
  Ebitand exp1 exp2 -> failure x
  Eeq exp1 exp2 -> failure x
  Eneq exp1 exp2 -> failure x
  Elthen exp1 exp2 -> failure x
  Egrthen exp1 exp2 -> failure x
  Ele exp1 exp2 -> failure x
  Ege exp1 exp2 -> failure x
  Eleft exp1 exp2 -> failure x
  Eright exp1 exp2 -> failure x
  Eplus exp1 exp2 -> failure x
  Eminus exp1 exp2 -> failure x
  Etimes exp1 exp2 -> failure x
  Ediv exp1 exp2 -> failure x
  Emod exp1 exp2 -> failure x
  InnerExp popenparenthesis exp pcloseparenthesis -> failure x
  Evar pident -> failure x
  Econst constant -> failure x
  Estring pstring -> failure x
transConstant :: Constant -> Result
transConstant x = case x of
  Efloat pdouble -> failure x
  Echar pchar -> failure x
  Eint pinteger -> failure x

