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


tacGeneratorDeclaration x =
  case x of
    NoAssgmDec {} -> get
    AssgmDec ids _ exp -> tacGeneratorIdentifiers ids exp
    AssgmTypeDec ids _ _ _ exp -> tacGeneratorIdentifiers ids exp
    NoAssgmArrayFixDec {} -> get 
    NoAssgmArrayDec  {} -> get
    AssgmArrayTypeDec ids _ _ _ _ exp -> tacGeneratorIdentifiers ids exp
    AssgmArrayDec ids _ _ _ exp ->  tacGeneratorIdentifiers ids exp

tacGeneratorIdentifiers= tacGeneratorDeclExpression 0 0

addTacEntries tacEntry env@(tac,_te, _t, _b ) =
    (tacEntry ++ tac,_te, _t,_b)

addTacEntry tacEntry env@(tac,_te, _t, _b ) =
    (tacEntry:tac,_te, _t,_b)

tacGeneratorDeclExpression depth length ids decExp = case decExp of
  ExprDecArray arrays@(ArrayInit _ (x:xs) _ ) -> do 
    tacGeneratorDeclExpression (depth + 1) length ids x
    mapM_ (tacGeneratorDeclExpression depth (length + 1) ids) xs
    get
  ExprDec exp -> do
    (tacEntry, temp) <- tacGeneratorExpression exp
    modify $ addTacEntries tacEntry
    if depth == 0
      then do
        mapM_ (tacGeneratorIdentifier temp) ids
        get
      else 
        mapM_ (tacGeneratorArrayIdentifier temp (getExpPos exp) depth) ids
        get
    get


tacGeneratorIdentifier temp@(Temp _ _ ty) id@(PIdent (loc, identifier)) = do
  modify $ addTacEntry $ TACEntry Nothing loc $ Nullary (Temp identifier loc ty) temp 
  get

tacGeneratorArrayIdentifier temp@(Temp _ _ ty) expLoc actual id@(PIdent (loc, identifier)) = do
  modify $ addTacEntry $ TACEntry Nothing expLoc $ IndexLeft (Temp identifier loc ty) (Temp (show actual) loc Int) temp
  get


tacGeneratorArrayExpression' exp = return ([], Temp "pippo" (10,10) Float)

tacGeneratorExpression:: Exp -> TacMonad ([TACEntry], Temp) 
tacGeneratorExpression exp = case exp of
    --EAss e1 assign e2 -> do
      -- (tac1,Temp id1 loc1 ty1) <- tacGeneratorExpression e1
      -- (tac2,Temp id2 loc2 ty2) <- tacGeneratorExpression e2
      -- idTemp <- newtemp
      -- let temp = Temp idTemp (getAssignPos assign) (tacSup ty1 ty2)
      --     newEntry = TACEntry Nothing (getAssignPos assign) (Binary temp (Temp id1 loc1 ty1) (Bop Int Plus) (Temp id2 loc2 ty2))
      --     tac = newEntry:(tac1 ++ tac2) in 
      --       return (tac, temp)
    Eplus e1 (PEplus (loc,_)) e2 -> tacGeneratorExpression' e1 Plus e2 loc
    Emod e1 (PEmod (loc,_)) e2 -> tacGeneratorExpression' e1 Modul e2 loc
    Eminus e1 (PEminus (loc,_)) e2 -> tacGeneratorExpression' e1 Minus e2 loc
    Ediv e1 (PEdiv (loc,_)) e2 ->  tacGeneratorExpression' e1 Div e2 loc
    Etimes e1 (PEtimes (loc,_)) e2 -> tacGeneratorExpression' e1 Times e2 loc
    InnerExp _ e _ -> tacGeneratorExpression e
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
    Evar identifier@(PIdent (loc, id)) -> return ([], Temp id loc Int) --getVarType identifier environment
    Econst (Estring (PString (loc, id))) -> return ([], Temp id loc String)
    Econst (Eint (PInteger (loc, id))) -> return ([],  Temp id loc Int)
    Econst (Efloat (PDouble (loc, id))) -> return ([], Temp id loc Float)
    Econst (Echar (PChar (loc, id))) -> return ([], Temp id loc Char)
    Econst (ETrue (PTrue (loc,id))) -> return ([],  Temp id loc Bool)
    Econst (EFalse (PFalse (loc,id))) -> return ([], Temp id loc Bool)

tacGeneratorExpression':: Exp -> BopType -> Exp -> Loc ->  TacMonad ([TACEntry], Temp) 
tacGeneratorExpression' e1 op e2 loc = do
  (tac1,temp1@(Temp _ _ ty1)) <- tacGeneratorExpression e1
  (tac2,temp2@(Temp _ _ ty2)) <- tacGeneratorExpression e2
  idTemp <- newtemp
  let temp = Temp idTemp loc (tacSup ty1 ty2)
      newEntry = TACEntry Nothing loc (Binary temp temp1 (Bop Int op) temp2)
      tac = newEntry:(tac1 ++ tac2) in 
        return (tac, temp)

newtemp:: TacMonad String
newtemp = do
  (_tac, _temp , k , _bp) <- get
  put (_tac, _temp , k + 1 , _bp)
  return $ int2AddrTempName k

int2AddrTempName k = "t" ++ show k