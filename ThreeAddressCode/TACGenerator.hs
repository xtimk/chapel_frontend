module ThreeAddressCode.TACGenerator where
import ThreeAddressCode.TAC

import AbsChapel
import Control.Monad.Trans.State
import qualified Data.Map as DMap

startTacState bpTree = ([], 0, bpTree)

tacGenerator (Progr p) = tacGeneratorModule p

tacGeneratorModule (Mod m) = do
  tacGeneratorExt m
  get

tacGeneratorExt [] = get
tacGeneratorExt (x:xs) = case x of
    ExtDecl (Decl decMode declList _ ) -> do
        mapM_ tacGeneratorDeclaration declList
        tacGeneratorExt xs
    ExtFun fun -> -- do
        --tacGeneratorFunction fun
        tacGeneratorExt xs

tacGeneratorDeclaration x = do
  environment <- get
  case x of
    NoAssgmDec ids _colon types -> get
    AssgmDec ids assignment exp ->
      let TacChecker tacEntries temp = tacGeneratorDeclExpression environment exp in do
            modify $ addTacEntries tacEntries
            tacGeneratorIdentifiers temp ids
            get
    AssgmTypeDec ids _colon types assignment exp -> get
      --let DataChecker ty errors = typeCheckerDeclExpression environment exp in do
      --      typeCheckerIdentifiers ids (Just assignment) (convertTypeSpecToTypeInferred types) ty
      --      modify $ addErrorsCurrentNode errors
      --      get
    NoAssgmArrayFixDec ids _colon array -> get --typeCheckerIdentifiersArray ids Nothing array Infered Infered
    NoAssgmArrayDec ids _colon array types -> get --typeCheckerIdentifiersArray ids  Nothing array (convertTypeSpecToTypeInferred types) Infered
    AssgmArrayTypeDec ids _colon array types assignment exp ->  get
      --let DataChecker ty errors = typeCheckerDeclExpression environment exp in do
       --     typeCheckerIdentifiersArray ids (Just assignment) array (convertTypeSpecToTypeInferred types) ty
       --    modify $ addErrorsCurrentNode errors
        --    get
    AssgmArrayDec ids _colon array assignment exp ->  get
     -- let DataChecker ty errors = typeCheckerDeclExpression environment exp in do
       --     typeCheckerIdentifiersArray ids (Just assignment) array Infered ty
         --   modify $ addErrorsCurrentNode errors
         --   get

tacGeneratorIdentifiers temp = mapM_ (tacGeneratorIdentifier temp)
tacGeneratorIdentifier temp id@(PIdent (loc, identifier)) = do
    modify $ addTacEntry $ TACEntry Nothing loc $ Nullary (Temp identifier loc) temp
    get

addTacEntries tacEntry env@(tac, _t, _b ) =
    (tacEntry ++ tac, _t,_b)

addTacEntry tacEntry env@(tac, _t, _b ) =
    (tacEntry:tac, _t,_b)


tacGeneratorDeclExpression environment decExp = case decExp of
  ExprDecArray arrays@(ArrayInit _ expression@(exp:exps) _) ->  TacChecker [] $ Temp "pippo" (0,0) -- foldl (typeCheckerDeclArrayExp environment) (DataChecker (Array Infered (Fix 0, Fix 0)) []) expression
  ExprDec exp -> tacGeneratorExpression environment exp --Temp "pippo" (0,0)

tacGeneratorExpression environment exp = case exp of
    EAss e1 assign e2 -> TacChecker [] $ Temp "pippo" (0,0) --typeCheckerAssignment environment e1 assign e2
    Eplus e1 (PEplus (loc,_)) e2 -> TacChecker [] $ Temp "pippo" (0,0)--typeCheckerExpression' environment SupMinus loc e1 e2
    Emod e1 (PEmod (loc,_)) e2 -> TacChecker [] $ Temp "pippo" (0,0)-- typeCheckerExpression' environment SupMod loc e1 e2
    Eminus e1 (PEminus (loc,_)) e2 -> TacChecker [] $ Temp "pippo" (0,0)--typeCheckerExpression' environment SupArith loc e1 e2
    Ediv e1 (PEdiv (loc,_)) e2 -> TacChecker [] $ Temp "pippo" (0,0)-- typeCheckerExpression' environment SupArith loc e1 e2
    Etimes e1 (PEtimes (loc,_)) e2 -> TacChecker [] $ Temp "pippo" (0,0)--typeCheckerExpression' environment SupArith loc e1 e2
    InnerExp _ e _ -> TacChecker [] $ Temp "pippo" (0,0) --typeCheckerExpression environment e
    Elthen e1 pElthen e2 -> TacChecker [] $ Temp "pippo" (0,0)-- typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Elor e1 pElor e2 -> TacChecker [] $ Temp "pippo" (0,0)--typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Eland e1 pEland e2 -> TacChecker [] $ Temp "pippo" (0,0)--typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Eneq e1 pEneq e2 -> TacChecker [] $ Temp "pippo" (0,0)--typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Eeq e1 pEeq e2 -> TacChecker [] $ Temp "pippo" (0,0)-- typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Egrthen e1 pEgrthen e2 -> TacChecker [] $ Temp "pippo" (0,0)-- typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Ele e1 pEle e2 -> TacChecker [] $ Temp "pippo" (0,0)--typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Ege e1 pEge e2 -> TacChecker [] $ Temp "pippo" (0,0)--typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Epreop (Indirection _) e1 -> TacChecker [] $ Temp "pippo" (0,0)-- typeCheckerIndirection environment e1
    Epreop (Address _) e1 -> TacChecker [] $ Temp "pippo" (0,0)-- typeCheckerAddress environment e1
    Earray expIdentifier arDeclaration -> TacChecker [] $ Temp "pippo" (0,0)--typeCheckerDeclarationArray  environment expIdentifier arDeclaration
    EFun funidentifier _ passedparams _ -> TacChecker [] $ Temp "pippo" (0,0)--eFunTypeChecker funidentifier passedparams environment
    Evar identifier -> TacChecker [] $ Temp "pippo" (0,0)--getVarType identifier environment
    Econst (Estring _) -> TacChecker [] $ Temp "pippo" (0,0)--DataChecker String []
    Econst (Eint (PInteger (loc, id))) -> TacChecker [] $ Temp id loc-- DataChecker Int []
    Econst (Efloat (PDouble (loc, id))) -> TacChecker [] $ Temp id loc --DataChecker Real []
    Econst (Echar _) -> TacChecker [] $ Temp "pippo" (0,0)--DataChecker Char []
    Econst (ETrue (PTrue (loc,id))) -> TacChecker [] $ Temp id loc --DataChecker Bool []
    Econst (EFalse _) -> TacChecker [] $ Temp "pippo" (0,0)--DataChecker Bool []


