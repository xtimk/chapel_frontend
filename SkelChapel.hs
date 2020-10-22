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
transPOpenBracket :: POpenBracket -> Result
transPOpenBracket x = case x of
  POpenBracket string -> failure x
transPCloseBracket :: PCloseBracket -> Result
transPCloseBracket x = case x of
  PCloseBracket string -> failure x
transPSemicolon :: PSemicolon -> Result
transPSemicolon x = case x of
  PSemicolon string -> failure x
transPColon :: PColon -> Result
transPColon x = case x of
  PColon string -> failure x
transPPoint :: PPoint -> Result
transPPoint x = case x of
  PPoint string -> failure x
transPIf :: PIf -> Result
transPIf x = case x of
  PIf string -> failure x
transPThen :: PThen -> Result
transPThen x = case x of
  PThen string -> failure x
transPElse :: PElse -> Result
transPElse x = case x of
  PElse string -> failure x
transPdo :: Pdo -> Result
transPdo x = case x of
  Pdo string -> failure x
transPWhile :: PWhile -> Result
transPWhile x = case x of
  PWhile string -> failure x
transPInt :: PInt -> Result
transPInt x = case x of
  PInt string -> failure x
transPReal :: PReal -> Result
transPReal x = case x of
  PReal string -> failure x
transPAssignmEq :: PAssignmEq -> Result
transPAssignmEq x = case x of
  PAssignmEq string -> failure x
transPAssignmPlus :: PAssignmPlus -> Result
transPAssignmPlus x = case x of
  PAssignmPlus string -> failure x
transPRef :: PRef -> Result
transPRef x = case x of
  PRef string -> failure x
transPVar :: PVar -> Result
transPVar x = case x of
  PVar string -> failure x
transPConst :: PConst -> Result
transPConst x = case x of
  PConst string -> failure x
transPProc :: PProc -> Result
transPProc x = case x of
  PProc string -> failure x
transPReturn :: PReturn -> Result
transPReturn x = case x of
  PReturn string -> failure x
transPElthen :: PElthen -> Result
transPElthen x = case x of
  PElthen string -> failure x
transPEgrthen :: PEgrthen -> Result
transPEgrthen x = case x of
  PEgrthen string -> failure x
transPEplus :: PEplus -> Result
transPEplus x = case x of
  PEplus string -> failure x
transPEminus :: PEminus -> Result
transPEminus x = case x of
  PEminus string -> failure x
transPEtimes :: PEtimes -> Result
transPEtimes x = case x of
  PEtimes string -> failure x
transPEdiv :: PEdiv -> Result
transPEdiv x = case x of
  PEdiv string -> failure x
transPEmod :: PEmod -> Result
transPEmod x = case x of
  PEmod string -> failure x
transPDef :: PDef -> Result
transPDef x = case x of
  PDef string -> failure x
transPElor :: PElor -> Result
transPElor x = case x of
  PElor string -> failure x
transPEland :: PEland -> Result
transPEland x = case x of
  PEland string -> failure x
transPEeq :: PEeq -> Result
transPEeq x = case x of
  PEeq string -> failure x
transPEneq :: PEneq -> Result
transPEneq x = case x of
  PEneq string -> failure x
transPEle :: PEle -> Result
transPEle x = case x of
  PEle string -> failure x
transPEge :: PEge -> Result
transPEge x = case x of
  PEge string -> failure x
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
  Decl decmode decllists psemicolon -> failure x
transDeclList :: DeclList -> Result
transDeclList x = case x of
  NoAssgmDec pidents pcolon type_ -> failure x
  NoAssgmArrayFixDec pidents pcolon ardecl -> failure x
  NoAssgmArrayDec pidents pcolon ardecl type_ -> failure x
  AssgmTypeDec pidents pcolon type_ passignmeq exprdecl -> failure x
  AssgmArrayTypeDec pidents pcolon ardecl type_ passignmeq exprdecl -> failure x
  AssgmArrayDec pidents pcolon ardecl passignmeq exprdecl -> failure x
  AssgmDec pidents passignmeq exprdecl -> failure x
transExprDecl :: ExprDecl -> Result
transExprDecl x = case x of
  ExprDecArray arinit -> failure x
  ExprDec exp -> failure x
transArInit :: ArInit -> Result
transArInit x = case x of
  ArrayInit popenbracket exprdecls pclosebracket -> failure x
transArDecl :: ArDecl -> Result
transArDecl x = case x of
  ArrayDeclIndex popenbracket ardims pclosebracket -> failure x
  ArrayDeclFixed popenbracket arbound pclosebracket -> failure x
transArDim :: ArDim -> Result
transArDim x = case x of
  ArrayDimSingle arbound1 ppoint1 ppoint2 arbound2 -> failure x
transArBound :: ArBound -> Result
transArBound x = case x of
  ArrayBoundIdent pident -> failure x
  ArratBoundConst constant -> failure x
transDecMode :: DecMode -> Result
transDecMode x = case x of
  PVarMode pvar -> failure x
  PConstMode pconst -> failure x
transFunction :: Function -> Result
transFunction x = case x of
  FunDec pproc signature body -> failure x
transSignature :: Signature -> Result
transSignature x = case x of
  SignNoRet pident functionparams -> failure x
  SignWRet pident functionparams pcolon type_ -> failure x
transFunctionParams :: FunctionParams -> Result
transFunctionParams x = case x of
  FunParams popenparenthesis params pcloseparenthesis -> failure x
transParam :: Param -> Result
transParam x = case x of
  ParNoMode pidents pcolon type_ -> failure x
  ParWMode mode pidents pcolon type_ -> failure x
transBody :: Body -> Result
transBody x = case x of
  BodyBlock popengraph bodystatements pclosegraph -> failure x
transBodyStatement :: BodyStatement -> Result
transBodyStatement x = case x of
  Stm statement -> failure x
  Fun function psemicolon -> failure x
  DeclStm declaration -> failure x
  Block body -> failure x
transStatement :: Statement -> Result
transStatement x = case x of
  DoWhile pdo pwhile body guard -> failure x
  While pwhile guard body -> failure x
  If pif guard pthen body -> failure x
  IfElse pif guard pthen body1 pelse body2 -> failure x
  RetVal preturn exp psemicolon -> failure x
  RetVoid preturn psemicolon -> failure x
  StExp exp psemicolon -> failure x
transGuard :: Guard -> Result
transGuard x = case x of
  SGuard popenparenthesis exp pcloseparenthesis -> failure x
transType :: Type -> Result
transType x = case x of
  Tint pint -> failure x
  Treal preal -> failure x
transAssgnmOp :: AssgnmOp -> Result
transAssgnmOp x = case x of
  AssgnEq passignmeq -> failure x
  AssgnPlEq passignmplus -> failure x
transMode :: Mode -> Result
transMode x = case x of
  RefMode pref -> failure x
transExp :: Exp -> Result
transExp x = case x of
  EAss exp1 assgnmop exp2 -> failure x
  Elor exp1 pelor exp2 -> failure x
  Eland exp1 peland exp2 -> failure x
  Ebitand exp1 pdef exp2 -> failure x
  Eeq exp1 peeq exp2 -> failure x
  Eneq exp1 peneq exp2 -> failure x
  Elthen exp1 pelthen exp2 -> failure x
  Egrthen exp1 pegrthen exp2 -> failure x
  Ele exp1 pele exp2 -> failure x
  Ege exp1 pege exp2 -> failure x
  Eplus exp1 peplus exp2 -> failure x
  Eminus exp1 peminus exp2 -> failure x
  Etimes exp1 petimes exp2 -> failure x
  Ediv exp1 pediv exp2 -> failure x
  Emod exp1 pemod exp2 -> failure x
  Epreop unaryoperator exp -> failure x
  Earray exp arinit -> failure x
  InnerExp popenparenthesis exp pcloseparenthesis -> failure x
  Evar pident -> failure x
  Econst constant -> failure x
  Estring pstring -> failure x
transUnaryOperator :: UnaryOperator -> Result
transUnaryOperator x = case x of
  Address pdef -> failure x
  Indirection petimes -> failure x
transConstant :: Constant -> Result
transConstant x = case x of
  Efloat pdouble -> failure x
  Echar pchar -> failure x
  Eint pinteger -> failure x

