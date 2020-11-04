module ThreeAddressCode.TACGenerator where
import ThreeAddressCode.TAC

import AbsChapel
import Checker.BPTree
import Control.Monad.Trans.State
import qualified Data.Map as DMap
import Utils.AbsUtils

type TacMonad a = State ([TACEntry], Temp, Int, BPTree BP) a


startTacState bpTree = ([], Temp Fix "" (0::Int,0::Int) ThreeAddressCode.TAC.Int,  0, bpTree)

addTacEntries tacEntry env@(tac,_te, _t, _b ) =
    (tacEntry ++ tac,_te, _t,_b)

addTacEntry tacEntry env@(tac,_te, _t, _b ) =
    (tacEntry:tac,_te, _t,_b)

tacGenerator (Progr p) = tacGeneratorModule p

tacGeneratorModule (Mod m) = do
  tacGeneratorExt m
  get

tacGeneratorExt [] = get
tacGeneratorExt (x:xs) = case x of
    ExtDecl (Decl decMode declList _ ) -> do
        mapM_ tacGeneratorDeclaration declList
        tacGeneratorExt xs
    ExtFun fun -> do
        tacGeneratorFunction fun
        tacGeneratorExt xs


tacGeneratorDeclaration x =
  case x of
    NoAssgmDec {} -> get
    AssgmDec ids _ exp -> tacGeneratorDeclExpression ids exp
    AssgmTypeDec ids _ _ _ exp -> tacGeneratorDeclExpression ids exp
    NoAssgmArrayFixDec {} -> get 
    NoAssgmArrayDec  {} -> get
    AssgmArrayTypeDec ids _ _ _ _ exp -> tacGeneratorDeclExpression ids exp
    AssgmArrayDec ids _ _ _ exp ->  tacGeneratorDeclExpression ids exp

tacGeneratorDeclExpression = tacGeneratorDeclExpression' 0 0

tacGeneratorDeclExpression' depth length ids decExp = 
  case decExp of 
    ExprDecArray (ArrayInit _ expressions _ ) -> tacGeneratorDeclExpression'' depth length ids expressions
    ExprDec exp -> do
      (tacEntry, temp) <- tacGeneratorExpression exp
      modify $ addTacEntries tacEntry
      if depth == 0
        then mapM_ (tacGeneratorIdentifier temp) ids
        else mapM_ (tacGeneratorArrayIdentifier temp (getExpPos exp) depth length ) ids
      get

tacGeneratorDeclExpression'' _ _ _ [] = get
tacGeneratorDeclExpression'' depth length ids (x:xs) = do
  tacGeneratorDeclExpression' (depth + 1) length ids x
  tacGeneratorDeclExpression'' depth (length + 1) ids xs
  get


tacGeneratorIdentifier temp@(Temp _ _ _ ty) (PIdent (loc, identifier)) = do
  modify $ addTacEntry $ TACEntry Nothing loc $ Nullary (Temp Var identifier loc ty) temp 
  get

tacGeneratorArrayIdentifier temp@(Temp _ _ _ ty) expLoc depth lenght (PIdent (loc, identifier)) = do
  modify $ addTacEntry $ TACEntry Nothing expLoc $ IndexLeft (Temp Var identifier loc ty) (Temp Fix (show lenght) loc Int) temp
  get


tacGeneratorFunction (FunDec (PProc (loc@(l,c),funname) ) signature body@(BodyBlock  (POpenGraph (locGraph,_)) _ _)) = 
  tacGeneratorBody  body
  --tacGeneratorBody ProcedureBlk (createId' l c (getFunName signature)) body

--tacGeneratorBody blkType identifier (BodyBlock  (POpenGraph (locStart,_)) xs (PCloseGraph (locEnd,_))  ) = do
tacGeneratorBody (BodyBlock  (POpenGraph (locStart,_)) xs (PCloseGraph (locEnd,_))  ) = do
  mapM_ tacGeneratorBody' xs
  get

tacGeneratorBody' x =
  case x of
    Stm statement -> tacGeneratorStatement statement
    Fun fun _ -> tacGeneratorFunction fun
    DeclStm (Decl decMode declList _ ) -> do
      mapM_ tacGeneratorDeclaration declList
      get
    Block body@(BodyBlock (POpenGraph ((l,c), name)) _ _) -> tacGeneratorBody body


tacGeneratorStatement statement = case statement of
  Break (PBreak (pos@(l,c), name)) _semicolon -> --do
    --typeCheckerSequenceStatement pos
    get
  Continue (PContinue (pos@(l,c), name)) _semicolon -> --do
    --typeCheckerSequenceStatement pos
    get 
  DoWhile (Pdo ((l,c),name)) _while body guard -> --do
    --typeCheckerBody DoWhileBlk (createId l c name) body
    --typeCheckerGuard guard
    get
  While (PWhile ((l,c), name)) guard body -> --do
    --typeCheckerGuard guard
    --typeCheckerBody WhileBlk (createId l c name) body
    get
  If (PIf ((l,c), name)) guard _then body -> --do
    --typeCheckerGuard guard
    --typeCheckerBody IfSimpleBlk (createId l c name) body
    get
  IfElse (PIf ((lIf,cIf), nameIf)) guard _then bodyIf (PElse ((lElse,cElse), nameElse)) bodyElse -> --do
    --typeCheckerGuard guard
    --typeCheckerBody IfThenBlk (createId lIf cIf nameIf) bodyIf
    --typeCheckerBody IfElseBlk (createId lElse cElse nameElse) bodyElse
    get
  StExp exp@(EFun funidentifier _ params _) _semicolon-> --do
    --environment <- get
    --let DataChecker ty errors = eFunTypeChecker funidentifier params environment in
     -- modify $ addErrorsCurrentNode errors
    get
  StExp exp _semicolon -> do
    (tacEntry, temp) <- tacGeneratorExpression exp
    modify $ addTacEntries tacEntry
    get
    --env <- get
   -- let DataChecker _ errors = typeCheckerExpression env exp in do
       -- modify $ addErrorsCurrentNode errors
       -- get
  RetVal return exp _semicolon -> get --do
    --(_s,tree,current_id) <- get
    --let DataChecker tyret errs2 = typeCheckerExpression (_s,tree,current_id) exp in do
     -- modify $ addErrorsCurrentNode errs2
     -- typeCheckerReturn return tyret
  RetVoid return _semicolon -> get --typeCheckerReturn return Checker.SymbolTable.Void

tacGeneratorExpression :: Exp -> TacMonad ([TACEntry], Temp) 
tacGeneratorExpression exp = case exp of
    EAss e1 assign e2 -> tacGeneratorAssignment e1 assign e2
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
    Earray expIdentifier arDeclaration -> do
       (tacId, tempId) <- tacGeneratorExpression expIdentifier
       (tac, temp) <- tacGeneratorArrayIndexing tempId expIdentifier arDeclaration
       modify $ addTacEntries (tacId ++ tac)
       return ([], temp)
    -- EFun funidentifier _ passedparams _ -> TacChecker [] $ Temp "pippo" (0,0) Bool--eFunTypeChecker funidentifier passedparams environment
    Evar identifier@(PIdent (loc, id)) -> return ([], Temp Var id loc Int) --getVarType identifier environment
    Econst (Estring (PString desc)) ->tacGeneratorConstant desc String
    Econst (Eint (PInteger desc)) -> tacGeneratorConstant desc Int
    Econst (Efloat (PDouble desc)) -> tacGeneratorConstant desc Float
    Econst (Echar (PChar desc)) -> tacGeneratorConstant desc Char
    Econst (ETrue (PTrue desc)) -> tacGeneratorConstant desc Bool
    Econst (EFalse (PFalse desc)) -> tacGeneratorConstant desc Bool

tacGeneratorConstant (loc, id) ty = return ([], Temp Fix id loc ty)

tacGeneratorArrayIndexing tempId exp (ArrayInit _ xs _ ) = do
  id <- newtemp 
  (tac,temp) <- tacGeneratorArrayIndexing' 0 (Temp Var id (getExpPos exp) Int) xs
  id <- newtemp 
  let indexTemp = (Temp Var id (getExpPos exp) Int)
      newEntry = TACEntry Nothing (-1,-1) $ IndexRight indexTemp tempId temp in do
        modify $ addTacEntry newEntry
        return ([], indexTemp )

tacGeneratorArrayIndexing' _ temp [] = return ([],temp) 
tacGeneratorArrayIndexing' depht temp ((ExprDec exp):xs) = do
  (tacId, tempId) <- tacGeneratorExpression exp
  modify $ addTacEntries tacId
  if depht == 0
    then 
      tacGeneratorArrayIndexing' (depht + 1) tempId xs
    else do
      id <- newtemp 
      let addTemp = Temp Var id (getExpPos exp) Int
          newEntry = TACEntry Nothing (-1,-1) $ Binary addTemp tempId Plus temp in do
            modify $ addTacEntry newEntry
            tacGeneratorArrayIndexing' (depht + 1) addTemp xs
            

  

tacGeneratorAssignment leftExp assign rightExp = do
  (tacRight,tempRight) <- tacGeneratorExpression rightExp
  (tacLeft, operation, (Temp _ _ locLeft tyLeft)) <- tacGeneratorLeftExpression leftExp assign tempRight
  modify $ addTacEntries (tacLeft ++ tacRight)
  idTemp <- newtemp
  let temp = Temp Var idTemp locLeft tyLeft in do
    case operation of 
      Nullary temp1 temp2 -> case assign of
        AssgnEq (PAssignmEq (loc,_)) ->  modify $ addTacEntry $ TACEntry Nothing loc operation
        AssgnPlEq (PAssignmPlus (loc,_)) ->
          let entryAdd = TACEntry Nothing loc $ Binary temp temp1 Plus temp2 in modify $ addTacEntry entryAdd
      IndexLeft variable@(Temp _ _ locLeft ty) index tempRight -> case assign of 
        AssgnEq (PAssignmEq (loc,_)) -> modify $ addTacEntry $ TACEntry Nothing loc operation
        AssgnPlEq (PAssignmPlus (loc,_)) -> do
          idTempAdd <- newtemp
          let tempAdd = Temp Var idTempAdd locLeft ty
              entryArrayValue = TACEntry Nothing loc $ IndexRight temp variable index
              entryadd = TACEntry Nothing loc $ Binary tempAdd temp Plus tempRight 
              entryFinal = TACEntry Nothing loc $ IndexLeft variable index tempAdd in 
                modify $ addTacEntries $ entryArrayValue:entryadd:entryFinal:[]
    return ([], temp)
                

tacGeneratorLeftExpression leftExp assign tempRight = case leftExp of
  Evar {} -> do
    (_, tempLeft) <- tacGeneratorExpression leftExp
    return ([], Nullary tempLeft tempRight, tempLeft)
  Earray expIdentifier arDeclaration -> do
    (_, tempLeft) <- tacGeneratorExpression expIdentifier
    --Index da vedere 
    return ([], IndexLeft tempLeft tempLeft tempRight, tempLeft)
  

tacGeneratorExpression' :: Exp -> Bop -> Exp -> Loc ->  TacMonad ([TACEntry], Temp) 
tacGeneratorExpression' e1 op e2 loc = do
  (tac1,temp1@(Temp _ _ _ ty1)) <- tacGeneratorExpression e1
  (tac2,temp2@(Temp _ _ _ ty2)) <- tacGeneratorExpression e2
  idTemp <- newtemp
  let temp = Temp Var idTemp loc (tacSup ty1 ty2)
      newEntry = TACEntry Nothing loc (Binary temp temp1 op temp2)
      tac = newEntry:(tac1 ++ tac2) in 
        return (tac, temp)

newtemp :: TacMonad String
newtemp = do
  (_tac, _temp , k , _bp) <- get
  put (_tac, _temp , k + 1 , _bp)
  return $ int2AddrTempName k

int2AddrTempName k = "t" ++ show k