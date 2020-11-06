module ThreeAddressCode.TACGenerator where
import ThreeAddressCode.TAC
import ThreeAddressCode.TACPrettyPrinter

import AbsChapel
import Control.Monad.Trans.State
import qualified Data.Map as DMap
import Utils.AbsUtils

import Checker.BPTree
import Checker.SymbolTable
import Utils.Type
import Checker.SupTable

type TacMonad a = State ([TACEntry], Temp, Int, BPTree BP, [Label], Maybe Label) a


startTacState bpTree = ([], Temp ThreeAddressCode.TAC.Fix "" (0::Int,0::Int) Int,  0, bpTree, [], Nothing)

addTacEntries tacEntry (tac,_te, _t, _b, _labels, _label ) =
    (tacEntry ++ tac,_te, _t,_b, _labels, _label)

addTacEntry tacEntry (tac,_te, _t, _b, _labels, _label ) =
    (tacEntry:tac,_te, _t,_b, _labels, _label)

addBTrueJmpLabel tacEntry (_tac,_te, _t, _b, _labels, _label ) =
    (_tac,_te, _t,_b, _labels, _label)

addBFalseJmpLabel tacEntry (_tac,_te, _t, _b, _labels, _label ) =
    (_tac,_te, _t,_b, _labels, _label)


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
  let Variable loc ty = getVarTypeTAC id tacEnv
      arrayPos = arrayCalculatePosition ty lenghts
      temp1 = Temp ThreeAddressCode.TAC.Var identifier loc ty
      temp2 = Temp ThreeAddressCode.TAC.Fix (show arrayPos) loc Int in
        return $ TACEntry Nothing $ IndexLeft temp1 temp2 temp

arrayCalculatePosition ty lengths = (sizeof ty) * (arrayCalculatePosition' (reverse (getArrayDimensions ty)) lengths)
  where
    arrayCalculatePosition' _ [x] =  x 
    arrayCalculatePosition' (y:ys) (x:xs) = x + y * (arrayCalculatePosition' ys xs)

getArrayDimensions (Array ty bound) = (calculateBound bound):(getArrayDimensions ty)  
getArrayDimensions _ = []

calculateBound (boundLeft, boundRight) = 
  let dimensionLeft = calculateBound' boundLeft
      dimensionRight = calculateBound' boundRight in
       dimensionRight - dimensionLeft 
          where calculateBound' bound = case bound of
                  Utils.Type.Fix lenght -> lenght
                  Utils.Type.Var _ -> 0

tacGeneratorFunction (FunDec (PProc (loc@(l,c),funname) ) signature body@(BodyBlock  (POpenGraph (locGraph,_)) _ _)) = tacGeneratorBody body 

tacGeneratorBody (BodyBlock  _ xs _  ) = do
  label <- popLabel
  res <- tacGeneratorBody' xs
  return $ attachLabelToFirstElem label res

tacGeneratorBody' [] = return []
tacGeneratorBody' (x:xs) = do
  res1 <- tacGeneratorBody'' x
  label <- popLabel 
  res2 <- tacGeneratorBody' xs
  return (res1 ++ attachLabelToFirstElem label res2)

tacGeneratorBody'' x = case x of
  Stm statement -> tacGeneratorStatement statement
  Fun fun _ -> tacGeneratorFunction fun
  DeclStm (Decl _ declList _ ) -> do
    result <- mapM tacGeneratorDeclaration declList
    return (concat result)
  Block body@(BodyBlock (POpenGraph ((l,c), name)) _ _) -> tacGeneratorBody body


-- ritorno una coppia (tacs, eventuale label da attaccare al tac successivo)
tacGeneratorStatement statement = case statement of
  Break (PBreak (pos@(l,c), name)) _semicolon -> --do
    --typeCheckerSequenceStatement pos
    return []
  Continue (PContinue (pos@(l,c), name)) _semicolon -> --do
    --typeCheckerSequenceStatement pos
    return [] 
  DoWhile (Pdo ((l,c),name)) _while body guard -> --do
    --typeCheckerBody DoWhileBlk (createId l c name) body
    --typeCheckerGuard guard
    return []
  While (PWhile ((l,c), name)) guard body -> --do
    --typeCheckerGuard guard
    --typeCheckerBody WhileBlk (createId l c name) body
    return []
  If (PIf _) (SGuard _ guard _) _then body@(BodyBlock(POpenGraph (locStart,_)) _ (PCloseGraph (locEnd,_))  ) -> do
    labelTrue <- newlabel locStart
    labelFalse <- newlabel locEnd
    (resg,_) <- tacGeneratorGuard guard labelTrue labelFalse
    pushLabel labelTrue
    res <- tacGeneratorBody body
    pushLabel labelFalse
    return (resg ++ res) 
  IfElse (PIf ((lIf,cIf), nameIf)) guard _then bodyIf (PElse ((lElse,cElse), nameElse)) bodyElse -> --do
    --typeCheckerGuard guard
    --typeCheckerBody IfThenBlk (createId lIf cIf nameIf) bodyIf
    --typeCheckerBody IfElseBlk (createId lElse cElse nameElse) bodyElse
    return []
  StExp (EFun funidentifier _ passedparams _) _semicolon-> do
    (tacs, _) <- tacGeneratorFunctionCall funidentifier passedparams
    return tacs
  StExp exp _semicolon -> do
    (tacEntry, _) <- tacGeneratorExpression exp
    return $ reverse tacEntry
  RetVal ret exp _semicolon -> return [] --do
    --(_s,tree,current_id) <- get
    --let DataChecker tyret errs2 = typeCheckerExpression (_s,tree,current_id) exp in do
     -- modify $ addErrorsCurrentNode errs2
     -- typeCheckerReturn return tyret
  RetVoid ret _semicolon -> return [] --typeCheckerReturn return Checker.SymbolTable.Void

attachLabelToFirstElem _ [] = []
attachLabelToFirstElem (Just label) ((TACEntry _l _e):xs) = (TACEntry (Just label) _e):xs
attachLabelToFirstElem Nothing xs = xs


tacGeneratorGuard guard labelTrue labelFalse = tacGeneratorExpression guard

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
    Earray expIdentifier arDeclaration -> tacGeneratorArrayIndexing expIdentifier arDeclaration
    EFun funidentifier _ passedparams _ -> tacGeneratorFunctionCall funidentifier passedparams
    Evar identifier@(PIdent (_, id)) -> do
      tacEnv <- get
      let Variable loc ty = getVarTypeTAC identifier tacEnv in
        return ([], Temp ThreeAddressCode.TAC.Var id loc ty)
    Econst (Estring (PString desc)) ->tacGeneratorConstant desc String
    Econst (Eint (PInteger desc)) -> tacGeneratorConstant desc Int
    Econst (Efloat (PDouble desc)) -> tacGeneratorConstant desc Real
    Econst (Echar (PChar desc)) -> tacGeneratorConstant desc Char
    Econst (ETrue (PTrue desc)) -> tacGeneratorConstant desc Bool
    Econst (EFalse (PFalse desc)) -> tacGeneratorConstant desc Bool

tacGeneratorConstant (loc, id) ty = return ([], Temp ThreeAddressCode.TAC.Fix id loc ty)

tacCheckerBinaryBoolean loc e1 op e2 = return ([], Temp ThreeAddressCode.TAC.Fix "pippo" (0,0) Int)

tacGeneratorFunctionCall identifier@(PIdent (_, id)) params = do
  tacParameters <- mapM tacGeneratorFunctionParameter params
  tacEnv <- get
  let tacExp = concat $ map fst tacParameters 
      tacPar = map snd tacParameters 
      tacs = tacPar ++ tacExp
      Function loc _ retTy = getVarTypeTAC identifier tacEnv
      parameterLenght = length params
      funTemp = Temp ThreeAddressCode.TAC.Var id loc retTy in
        case retTy of
          Utils.Type.Void -> let tacEntry = TACEntry Nothing $ CallProc funTemp parameterLenght in 
                      return (tacEntry:tacs,  funTemp)
          _ -> do
            idResFun <- newtemp
            let idResTemp = Temp ThreeAddressCode.TAC.Var idResFun loc retTy
                tacEntry = TACEntry Nothing $ CallFun idResTemp funTemp parameterLenght in
                  return (tacEntry:tacs, idResTemp)


tacGeneratorFunctionParameter (PassedPar exp) = do
  (tacs, temp) <- tacGeneratorExpression exp
  let tacEntry = TACEntry Nothing $ SetParam temp in
    return (tacs, tacEntry)
    

tacGeneratorArrayIndexing var@(Evar identifier@(PIdent (_, id))) arrayDecl = do
  tacEnv <- get
  let Variable loc ty = getVarTypeTAC identifier tacEnv in do
    (tacsAdd, temp) <- tacGeneratorArrayIndexing' var arrayDecl
    idArVal <- newtemp
    let tempArrayVal = Temp ThreeAddressCode.TAC.Var idArVal loc ty 
        tempId = Temp ThreeAddressCode.TAC.Var id loc ty
        tacEntry = TACEntry Nothing $ IndexRight tempArrayVal tempId temp in
          return (tacEntry:tacsAdd, tempArrayVal)

tacGeneratorArrayIndexing' (Evar identifier) arrayDecl = do
  tacEnv <- get
  let var = getVarTypeTAC identifier tacEnv in do
    (tacsPos, temps) <- tacGenerationArrayPosition arrayDecl
    (tacsAdd, temp) <- tacGenerationArrayPosAdd var temps
    return (tacsAdd ++ tacsPos, temp)

tacGeneratorAssignment leftExp assign rightExp = do
  (tacRight,tempRight) <- tacGeneratorExpression rightExp
  (tacLeft, operation, (Temp _ _ locLeft tyLeft)) <- tacGeneratorLeftExpression leftExp assign tempRight
  idTemp <- newtemp
  let tacs = tacRight ++ tacLeft 
      temp = Temp ThreeAddressCode.TAC.Var idTemp locLeft tyLeft in
      case operation of 
        Nullary temp1 temp2 -> case assign of
          AssgnEq (PAssignmEq _) -> return ((TACEntry Nothing operation):tacs, temp)
          AssgnPlEq (PAssignmPlus _) ->
            let entryAdd = TACEntry Nothing $ Binary temp temp1 Plus temp2 in return (entryAdd:tacs, temp)
        IndexLeft variable@(Temp _ _ _ ty) index tempRight -> case assign of 
          AssgnEq (PAssignmEq _) -> return ((TACEntry Nothing operation):tacs, temp)
          AssgnPlEq (PAssignmPlus (loc,_)) -> do
            idTempAdd <- newtemp
            let tempAdd = Temp ThreeAddressCode.TAC.Var idTempAdd loc ty
                entryArrayValue = TACEntry Nothing $ IndexRight temp variable index
                entryadd = TACEntry Nothing $ Binary tempAdd temp Plus tempRight 
                entryFinal = TACEntry Nothing $ IndexLeft variable index tempAdd in 
                  return ((entryFinal:entryadd:entryArrayValue:[]) ++ tacs, temp)
                

tacGeneratorLeftExpression leftExp assign tempRight = case leftExp of
  Evar {} -> do
    (_, tempLeft) <- tacGeneratorExpression leftExp
    return ([], Nullary tempLeft tempRight, tempLeft)
  Earray expIdentifier arDeclaration -> do
    (_, tempLeft) <- tacGeneratorExpression expIdentifier
    (tacsAdd, temp) <- tacGeneratorArrayIndexing' expIdentifier arDeclaration
    return (tacsAdd, IndexLeft tempLeft temp tempRight, tempLeft)

tacGenerationArrayPosition (ArrayInit _ expressions _ ) = tacGenerationArrayPosition' expressions
  where
    tacGenerationArrayPosition' [ExprDec x] = do
      (tacExp, tempExp) <- tacGeneratorExpression x 
      return (tacExp, [tempExp])
    tacGenerationArrayPosition' (ExprDec x:xs) = do
      (tacExp, tempExp) <- tacGeneratorExpression x 
      (tacOther, tempOthers) <- tacGenerationArrayPosition' xs
      return ((tacExp ++ tacOther), tempExp:tempOthers)

tacGenerationArrayPosAdd (Variable loc ty) temps = do
  (tacs, temp) <- tacGenerationArrayPosAdd' (tail (reverse (getArrayDimensions ty))) (reverse temps)
  idSize <- newtemp
  let size = sizeof ty
      tempSize = Temp ThreeAddressCode.TAC.Fix (show size) loc Int 
      tempResult =  Temp ThreeAddressCode.TAC.Var idSize loc Int 
      sizeEntry =  TACEntry Nothing $ Binary tempResult temp Times tempSize in 
        return (sizeEntry:tacs,tempResult)
          where
            tacGenerationArrayPosAdd' _ [x] = return ([], x)
            tacGenerationArrayPosAdd' (x:xs) (y@(Temp _ _ loc _):ys) = do
              (tacs, temp) <- tacGenerationArrayPosAdd' xs ys
              idMultTemp <- newtemp
              idAddTemp <- newtemp
              let tempLength = Temp ThreeAddressCode.TAC.Fix (show x) loc Int
                  tempMult = Temp ThreeAddressCode.TAC.Var idMultTemp loc Int
                  tempAdd = Temp ThreeAddressCode.TAC.Var idAddTemp loc Int
                  mulEntry = TACEntry Nothing $ Binary tempMult temp Times tempLength
                  addEntry =  TACEntry Nothing $ Binary tempAdd y Plus tempMult in
                    return (addEntry:mulEntry:tacs,tempAdd)

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
  (_tac, _temp , k , _bp, _labels, _label) <- get
  put (_tac, _temp , k + 1 , _bp, _labels, _label)
  return $ int2AddrTempName k

--newlabel :: Loc -> TacMonad Label
newlabel pos = do
  (_tac, _temp , k , _bp, _labels, _label) <- get
  put (_tac, _temp , k + 1 , _bp, _labels, _label)
  return $ Just ((int2Label k),pos)

popLabel :: TacMonad (Maybe Label)
popLabel = do
  (_tac, _temp , k , _bp, _labels, label) <- get
  put (_tac, _temp , k , _bp, _labels, Nothing)
  return label

pushLabel label = do
  (_tac, _temp , k , _bp, _labels, _) <- get
  put (_tac, _temp , k , _bp, _labels, label)
