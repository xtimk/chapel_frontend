module ThreeAddressCode.TACGenerator where
import ThreeAddressCode.TAC

import AbsChapel
import Control.Monad.Trans.State
import qualified Data.Map as DMap
import Utils.AbsUtils

import Checker.BPTree
import Checker.SymbolTable
import Utils.Type
import Checker.SupTable

type TacMonad a = State ([TACEntry], Temp, Int, BPTree BP) a


startTacState bpTree = ([], Temp ThreeAddressCode.TAC.Fix "" (0::Int,0::Int) Int,  0, bpTree)

addTacEntries tacEntry env@(tac,_te, _t, _b ) =
    (tacEntry ++ tac,_te, _t,_b)

addTacEntry tacEntry env@(tac,_te, _t, _b ) =
    (tacEntry:tac,_te, _t,_b)

tacGenerator (Progr p) = tacGeneratorModule p

tacGeneratorModule (Mod m) = do
  res <- tacGeneratorExt m
  modify $ addTacEntries $ concat res
  get

tacGeneratorExt [] = return []
tacGeneratorExt (x:xs) = case x of
    ExtDecl (Decl decMode declList _ ) -> do
        res <- mapM tacGeneratorDeclaration declList
        res2 <- tacGeneratorExt xs
        return $ res ++ res2
    ExtFun fun -> do
        resfun <- tacGeneratorFunction fun -- todo
        res <- tacGeneratorExt xs
        return (resfun:res)


tacGeneratorDeclaration x =
  case x of
    NoAssgmDec {} -> return []
    AssgmDec ids _ exp -> tacGeneratorDeclExpression ids exp
    AssgmTypeDec ids _ _ _ exp -> tacGeneratorDeclExpression ids exp
    NoAssgmArrayFixDec {} -> return [] 
    NoAssgmArrayDec  {} -> return []
    AssgmArrayTypeDec ids _ _ _ _ exp -> tacGeneratorDeclExpression ids exp
    AssgmArrayDec ids _ _ _ exp -> tacGeneratorDeclExpression ids exp

tacGeneratorDeclExpression = tacGeneratorDeclExpression' []

tacGeneratorDeclExpression' lengths ids decExp = 
  case decExp of 
    ExprDecArray (ArrayInit _ expressions _ ) -> tacGeneratorDeclExpression'' (0:lengths) ids expressions
    ExprDec exp -> do
      (tacEntry, temp) <- tacGeneratorExpression exp
      if null lengths
        then do
          res <- mapM (tacGeneratorIdentifier temp) ids
          return $ reverse (res ++ tacEntry)
        else do
          res <- mapM (tacGeneratorArrayIdentifier temp (getExpPos exp) lengths ) ids
          return $ reverse (res ++ tacEntry)
      

tacGeneratorDeclExpression'' _ _ [] = return []
tacGeneratorDeclExpression'' lengths@(x:xs) ids (y:ys) = do
  res1 <- tacGeneratorDeclExpression'' ((x+1):xs) ids ys
  res2 <- tacGeneratorDeclExpression' lengths ids y
  return (res1 ++ res2)
  

--Vedere array qua
tacGeneratorIdentifier temp@(Temp _ _ _ ty) (PIdent (loc, identifier)) =
  return $ TACEntry Nothing $ Nullary (Temp ThreeAddressCode.TAC.Var identifier loc ty) temp

tacGeneratorArrayIdentifier temp@(Temp _ _ _ ty) expLoc lenghts id@(PIdent (loc, identifier)) = do
  tacEnv <- get
  let (loc,ty) = getVarTypeTAC id tacEnv
      arrayPos = arrayCalculatePosition ty lenghts
      temp1 = Temp ThreeAddressCode.TAC.Var identifier loc ty
      temp2 = Temp ThreeAddressCode.TAC.Fix (show arrayPos) loc Int in
        return $ TACEntry Nothing $ IndexLeft temp1 temp2 temp

arrayCalculatePosition ty lengths = (sizeof ty) * (arrayCalculatePosition' (reverse (getArrayDimensions ty)) lengths)
  where
    arrayCalculatePosition' _ [x] =  x 
    arrayCalculatePosition' (y:ys) (x:xs) = x + y * (arrayCalculatePosition' ys xs)

getArrayDimensions ar@(Array ty _) = (calculateBound ar):(getArrayDimensions ty)  
getArrayDimensions ty = []

calculateBound (Array ty (boundLeft, boundRight)) = 
  let dimensionLeft = calculateBound' boundLeft
      dimensionRight = calculateBound' boundRight in
       dimensionRight - dimensionLeft 
          where calculateBound' bound = case bound of
                  Utils.Type.Fix lenght -> lenght
                  Utils.Type.Var _ -> 0

tacGeneratorFunction (FunDec (PProc (loc@(l,c),funname) ) signature body@(BodyBlock  (POpenGraph (locGraph,_)) _ _)) = tacGeneratorBody body

tacGeneratorBody (BodyBlock  (POpenGraph (locStart,_)) xs (PCloseGraph (locEnd,_))  ) = tacGeneratorBody' xs

tacGeneratorBody' [] = return []
tacGeneratorBody' (x:xs) =
  case x of
    Stm statement -> do
        (tacs, label) <- tacGeneratorStatement statement
        tacs2 <- tacGeneratorBody' xs
        -- attacco l'eventuale label che mi viene passata al prossimo blocco di tac
        return $ tacs ++ (attachLabelToFirstElem label tacs2)
    Fun fun _ -> 
      tacGeneratorFunction fun
    DeclStm (Decl decMode declList _ ) -> do
      res <- mapM tacGeneratorDeclaration declList
      res2 <- tacGeneratorBody' xs
      return $ (concat res) ++ res2
    Block body@(BodyBlock (POpenGraph ((l,c), name)) _ _) -> tacGeneratorBody body


-- ritorno una coppia (tacs, eventuale label da attaccare al tac successivo)
tacGeneratorStatement statement = case statement of
  Break (PBreak (pos@(l,c), name)) _semicolon -> --do
    --typeCheckerSequenceStatement pos
    return ([], Nothing)
  Continue (PContinue (pos@(l,c), name)) _semicolon -> --do
    --typeCheckerSequenceStatement pos
    return ([], Nothing) 
  DoWhile (Pdo ((l,c),name)) _while body guard -> --do
    --typeCheckerBody DoWhileBlk (createId l c name) body
    --typeCheckerGuard guard
    return ([], Nothing)
  While (PWhile ((l,c), name)) guard body -> --do
    --typeCheckerGuard guard
    --typeCheckerBody WhileBlk (createId l c name) body
    return ([], Nothing)
  If (PIf ((l,c), name)) guard _then body -> do
    res <- tacGeneratorBody body
    label <- newlabel
    label2 <- newlabel
    return $ ((attachLabelToFirstElem (Just (label, (l,c))) res), (Just (label2,(l,c)))) --uncomment
    -- return []
  IfElse (PIf ((lIf,cIf), nameIf)) guard _then bodyIf (PElse ((lElse,cElse), nameElse)) bodyElse -> --do
    --typeCheckerGuard guard
    --typeCheckerBody IfThenBlk (createId lIf cIf nameIf) bodyIf
    --typeCheckerBody IfElseBlk (createId lElse cElse nameElse) bodyElse
    return ([], Nothing)
  StExp exp@(EFun funidentifier _ params _) _semicolon-> --do
    --environment <- get
    --let DataChecker ty errors = eFunTypeChecker funidentifier params environment in
     -- modify $ addErrorsCurrentNode errors
    return ([], Nothing)
  StExp exp _semicolon -> do
    env <- get
    (tacEntry, temp) <- tacGeneratorExpression exp
    -- modify $ addTacEntries (tacEntry)
    return $ (reverse tacEntry, Nothing) --uncomment
    -- return []
    --env <- get
   -- let DataChecker _ errors = typeCheckerExpression env exp in do
       -- modify $ addErrorsCurrentNode errors
       -- get
  RetVal ret exp _semicolon -> return ([], Nothing) --do
    --(_s,tree,current_id) <- get
    --let DataChecker tyret errs2 = typeCheckerExpression (_s,tree,current_id) exp in do
     -- modify $ addErrorsCurrentNode errs2
     -- typeCheckerReturn return tyret
  RetVoid ret _semicolon -> return ([], Nothing)  --typeCheckerReturn return Checker.SymbolTable.Void

attachLabelToFirstElem (Just label) ((TACEntry _l _e):xs) = (TACEntry (Just label) _e):xs
attachLabelToFirstElem Nothing xs = xs

-- attachLabelToFirstElem label [] = []


tacGeneratorExpression exp = case exp of
    EAss e1 assign e2 -> tacGeneratorAssignment e1 assign e2
    Eplus e1 (PEplus (loc,_)) e2 -> tacGeneratorExpression' e1 Plus e2 loc
    Emod e1 (PEmod (loc,_)) e2 -> tacGeneratorExpression' e1 Modul e2 loc
    Eminus e1 (PEminus (loc,_)) e2 -> tacGeneratorExpression' e1 Minus e2 loc
    Ediv e1 (PEdiv (loc,_)) e2 ->  tacGeneratorExpression' e1 Div e2 loc
    Etimes e1 (PEtimes (loc,_)) e2 -> tacGeneratorExpression' e1 Times e2 loc
    InnerExp _ e _ -> tacGeneratorExpression e
    Elthen e1 (PElthen (loc,_)) e2 -> tacCheckerBinaryBoolean loc e1 ThreeAddressCode.TAC.LT e2
    Elor e1 (PElor (loc,_)) e2 -> tacCheckerBinaryBoolean loc e1 ThreeAddressCode.TAC.OR e2
    Eland e1 (PEland (loc,_)) e2 -> tacCheckerBinaryBoolean loc e1 ThreeAddressCode.TAC.AND e2
    Eneq e1 (PEneq (loc,_)) e2 -> tacCheckerBinaryBoolean loc e1 ThreeAddressCode.TAC.NEQ e2
    Eeq e1 (PEeq (loc,_)) e2 -> tacCheckerBinaryBoolean loc e1 ThreeAddressCode.TAC.EQ e2
    Egrthen e1 (PEgrthen (loc,_)) e2 -> tacCheckerBinaryBoolean loc e1 ThreeAddressCode.TAC.GT e2
    Ele e1 (PEle (loc,_)) e2 -> tacCheckerBinaryBoolean loc e1 ThreeAddressCode.TAC.LTE e2
    Ege e1 (PEge (loc,_)) e2 -> tacCheckerBinaryBoolean loc e1 ThreeAddressCode.TAC.GTE e2
    --Epreop (Negation _) e1 ->
    --Epreop (Indirection _) e1 -> TacChecker [] $ Temp "pippo" (0,0) Bool-- typeCheckerIndirection environment e1
    --Epreop (Address _) e1 -> TacChecker [] $ Temp "pippo" (0,0) Bool-- typeCheckerAddress environment e1
    Earray expIdentifier arDeclaration -> do
       (tacId, tempId) <- tacGeneratorExpression expIdentifier
       (tac, temp) <- tacGeneratorArrayIndexing tempId expIdentifier arDeclaration
       return (tac:tacId, temp)
    -- EFun funidentifier _ passedparams _ -> TacChecker [] $ Temp "pippo" (0,0) Bool--eFunTypeChecker funidentifier passedparams environment
    Evar identifier@(PIdent (loc, id)) -> do
      tacEnv <- get
      let (loc,ty) = getVarTypeTAC identifier tacEnv in
        return ([], Temp ThreeAddressCode.TAC.Var id loc ty)
    Econst (Estring (PString desc)) ->tacGeneratorConstant desc String
    Econst (Eint (PInteger desc)) -> tacGeneratorConstant desc Int
    Econst (Efloat (PDouble desc)) -> tacGeneratorConstant desc Real
    Econst (Echar (PChar desc)) -> tacGeneratorConstant desc Char
    Econst (ETrue (PTrue desc)) -> tacGeneratorConstant desc Bool
    Econst (EFalse (PFalse desc)) -> tacGeneratorConstant desc Bool


tacGeneratorConstant (loc, id) ty = return ([], Temp ThreeAddressCode.TAC.Fix id loc ty)

tacCheckerBinaryBoolean loc e1 op e2 = return ([], Temp ThreeAddressCode.TAC.Fix "pippo" (0,0) Int)

tacGeneratorArrayIndexing tempId exp (ArrayInit _ xs _ ) = do
  id <- newtemp 
  (tac,temp) <- tacGeneratorArrayIndexing' [] 0 (Temp ThreeAddressCode.TAC.Var id (getExpPos exp) Int) xs
  id <- newtemp 
  let indexTemp = (Temp ThreeAddressCode.TAC.Var id (getExpPos exp) Int)
      newEntry = TACEntry Nothing $ IndexRight indexTemp tempId temp in 
        return (newEntry, indexTemp )

tacGeneratorArrayIndexing' tacs _ temp [] = return (tacs,temp) 
tacGeneratorArrayIndexing' tacs depht temp ((ExprDec exp):xs) = do
  (tacId, tempId) <- tacGeneratorExpression exp
  modify $ addTacEntries tacId -- DA ELIMINARE!!!
  if depht == 0
    then 
      tacGeneratorArrayIndexing' (tacs ++ tacId) (depht + 1) tempId xs
    else do
      id <- newtemp 
      let addTemp = Temp ThreeAddressCode.TAC.Var id (getExpPos exp) Int
          newEntry = TACEntry Nothing $ Binary addTemp tempId Plus temp in 
            tacGeneratorArrayIndexing' (newEntry:(tacs ++ tacId)) (depht + 1) addTemp xs


tacGeneratorAssignment leftExp assign rightExp = do
  (tacRight,tempRight) <- tacGeneratorExpression rightExp
  (tacLeft, operation, (Temp _ _ locLeft tyLeft)) <- tacGeneratorLeftExpression leftExp assign tempRight
  idTemp <- newtemp
  let tacs = tacLeft ++ tacRight
      temp = Temp ThreeAddressCode.TAC.Var idTemp locLeft tyLeft in
      case operation of 
        Nullary temp1 temp2 -> case assign of
          AssgnEq (PAssignmEq (loc,_)) -> return ((TACEntry Nothing operation):tacs, temp)
          AssgnPlEq (PAssignmPlus (loc,_)) ->
            let entryAdd = TACEntry Nothing $ Binary temp temp1 Plus temp2 in return (entryAdd:tacs, temp)
        IndexLeft variable@(Temp _ _ locLeft ty) index tempRight -> case assign of 
          AssgnEq (PAssignmEq (loc,_)) -> return ((TACEntry Nothing operation):tacs, temp)
          AssgnPlEq (PAssignmPlus (loc,_)) -> do
            idTempAdd <- newtemp
            let tempAdd = Temp ThreeAddressCode.TAC.Var idTempAdd locLeft ty
                entryArrayValue = TACEntry Nothing $ IndexRight temp variable index
                entryadd = TACEntry Nothing $ Binary tempAdd temp Plus tempRight 
                entryFinal = TACEntry Nothing $ IndexLeft variable index tempAdd in 
                  return ((entryArrayValue:entryadd:entryFinal:[]) ++ tacs, temp)
   -- return ([], temp)
                

tacGeneratorLeftExpression leftExp assign tempRight = case leftExp of
  Evar {} -> do
    (_, tempLeft) <- tacGeneratorExpression leftExp
    return ([], Nullary tempLeft tempRight, tempLeft)
  Earray expIdentifier arDeclaration -> do
    (_, tempLeft) <- tacGeneratorExpression expIdentifier
    --Index da vedere 
    return ([], IndexLeft tempLeft tempLeft tempRight, tempLeft)

tacGeneratorExpression' e1 op e2 loc = do
  (tac1,temp1@(Temp _ _ _ ty1)) <- tacGeneratorExpression e1
  (tac2,temp2@(Temp _ _ _ ty2)) <- tacGeneratorExpression e2
  idTemp <- newtemp
  let temp = Temp ThreeAddressCode.TAC.Var idTemp loc (supTac ty1 ty2)
      newEntry = TACEntry Nothing (Binary temp temp1 op temp2)
      tac = newEntry:(tac1 ++ tac2) in 
        return (tac, temp)

newtemp :: TacMonad String
newtemp = do
  (_tac, _temp , k , _bp) <- get
  put (_tac, _temp , k + 1 , _bp)
  return $ int2AddrTempName k

newlabel :: TacMonad String
newlabel = do
  (_tac, _temp , k , _bp) <- get
  put (_tac, _temp , k + 1 , _bp)
  return $ int2Label k

int2AddrTempName k = "t" ++ show k
int2Label k = "L" ++ show k