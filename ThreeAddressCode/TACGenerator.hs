module ThreeAddressCode.TACGenerator where
import ThreeAddressCode.TAC

import AbsChapel
import Checker.BPTree
import Control.Monad.Trans.State
import qualified Data.Map as DMap

type TacMonad a = State ([TACEntry], Temp, Int, BPTree BP) a


startTacState bpTree = ([], Temp "" (0::Int,0::Int) ThreeAddressCode.TAC.Int,  0, bpTree)

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
    AssgmDec ids assignment exp -> do
            (tacEntry, temp) <- tacGeneratorDeclExpression exp
            modify $ addTacEntries tacEntry
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
 

tacGeneratorIdentifier temp@(Temp _ _ ty) id@(PIdent (loc, identifier)) = do
  modify $ addTacEntry $ TACEntry Nothing loc $ Nullary (Temp identifier loc ty) temp 
  get

addTacEntries tacEntry env@(tac,_te, _t, _b ) =
    (tacEntry ++ tac,_te, _t,_b)

addTacEntry tacEntry env@(tac,_te, _t, _b ) =
    (tacEntry:tac,_te, _t,_b)

tacGeneratorDeclExpression:: ExprDecl -> TacMonad ([TACEntry], Temp) 
tacGeneratorDeclExpression decExp = case decExp of
  ExprDecArray arrays@(ArrayInit _ expression@(exp:exps) _) -> return ([],Temp "pippo" (0,0) Bool) -- foldl (typeCheckerDeclArrayExp environment) (DataChecker (Array Infered (Fix 0, Fix 0)) []) expression
  --ExprDec exp -> return (tacGeneratorExpression exp) --([],Temp "pippo" (0,0) Bool)
    --pippo <- tacGeneratorExpression exp 
    --return pippo
tacGeneratorExpression:: Exp -> TacMonad ([TACEntry], Temp) 
tacGeneratorExpression exp = case exp of
    EAss e1 assign e2 -> do
      (tac1,Temp id1 loc1 ty1) <- tacGeneratorExpression e1
      (tac2,Temp id2 loc2 ty2) <- tacGeneratorExpression e2
      idTemp <- newtemp
      let temp = Temp idTemp (getAssignPos assign) (tacSup ty1 ty2)
          newEntry = TACEntry Nothing (getAssignPos assign) (Binary temp (Temp id1 loc1 ty1) (Bop Int Plus) (Temp id2 loc2 ty2))
          tac = newEntry:(tac1 ++ tac2) in 
            return (tac, temp)
    -- Eplus e1 (PEplus (loc,_)) e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool--typeCheckerExpression' environment SupMinus loc e1 e2
    -- Emod e1 (PEmod (loc,_)) e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool-- typeCheckerExpression' environment SupMod loc e1 e2
    -- Eminus e1 (PEminus (loc,_)) e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool--typeCheckerExpression' environment SupArith loc e1 e2
    -- Ediv e1 (PEdiv (loc,_)) e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool-- typeCheckerExpression' environment SupArith loc e1 e2
    -- Etimes e1 (PEtimes (loc,_)) e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool--typeCheckerExpression' environment SupArith loc e1 e2
    -- InnerExp _ e _ -> TacChecker [] $ Temp "pippo" (0,0)  Bool--typeCheckerExpression environment e
    -- Elthen e1 pElthen e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool-- typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    -- Elor e1 pElor e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool--typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    -- Eland e1 pEland e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool--typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    -- Eneq e1 pEneq e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool--typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    -- Eeq e1 pEeq e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool-- typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    -- Egrthen e1 pEgrthen e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool-- typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    -- Ele e1 pEle e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool--typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    -- Ege e1 pEge e2 -> TacChecker [] $ Temp "pippo" (0,0) Bool--typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    -- Epreop (Indirection _) e1 -> TacChecker [] $ Temp "pippo" (0,0) Bool-- typeCheckerIndirection environment e1
    -- Epreop (Address _) e1 -> TacChecker [] $ Temp "pippo" (0,0) Bool-- typeCheckerAddress environment e1
    -- Earray expIdentifier arDeclaration -> TacChecker [] $ Temp "pippo" (0,0) Bool--typeCheckerDeclarationArray  environment expIdentifier arDeclaration
    -- EFun funidentifier _ passedparams _ -> TacChecker [] $ Temp "pippo" (0,0) Bool--eFunTypeChecker funidentifier passedparams environment
    -- Evar identifier -> TacChecker [] $ Temp "pippo" (0,0) Bool--getVarType identifier environment
    Econst (Estring (PString (loc, id))) -> return ([], Temp id loc String)
    Econst (Eint (PInteger (loc, id))) -> return ([],  Temp id loc Int)
    Econst (Efloat (PDouble (loc, id))) -> return ([], Temp id loc Float)
    Econst (Echar (PChar (loc, id))) -> return ([], Temp id loc Char)
    Econst (ETrue (PTrue (loc,id))) -> return ([],  Temp id loc Bool)
    Econst (EFalse (PFalse (loc,id))) -> return ([], Temp id loc Bool)

  

newtemp:: TacMonad String
newtemp = do
  (_tac, _temp , k , _bp) <- get
  put (_tac, _temp , k + 1 , _bp)
  return $ int2AddrTempName k

int2AddrTempName k = "t" ++ show k