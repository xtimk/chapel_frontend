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
import ThreeAddressCode.TACStateUtils

tacGenerator (Progr p) = tacGeneratorModule p

tacGeneratorModule (Mod m) = do
  res <- tacGeneratorExt m
  modify $ addTacEntries $ concat res
  get

tacGeneratorExt [] = return []
tacGeneratorExt (x:xs) = case x of
    ExtDecl (Decl _dec declList _ ) -> do
        res <- mapM tacGeneratorDeclaration declList
        res2 <- tacGeneratorExt xs
        return $ res ++ res2
    ExtFun fun -> do
        resfun <- tacGeneratorFunction fun 
        tacsFun <- popFunTacs 
        res <- tacGeneratorExt xs
        return $ (tacsFun ++ resfun):res

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
      (tacEntry, temp) <- tacGeneratorExpression RightExp exp
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

tacGeneratorArrayIdentifier temp@(Temp _ _ _ _) _ lenghts id@(PIdent (_, identifier)) = do
  tacEnv <- get
  let Variable loc ty = getVarTypeTAC id tacEnv
  let arrayPos = arrayCalculatePosition ty lenghts
  let temp1 = Temp ThreeAddressCode.TAC.Var identifier loc ty
  let temp2 = Temp ThreeAddressCode.TAC.Fix (show arrayPos) loc Int
  return $ TACEntry Nothing $ IndexLeft temp1 temp2 temp

arrayCalculatePosition ty lengths = (sizeof ty) * (arrayCalculatePosition' (reverse (getArrayDimensions ty)) lengths)
  where
    arrayCalculatePosition' _ [x] =  x 
    arrayCalculatePosition' (y:ys) (x:xs) = x + y * (arrayCalculatePosition' ys xs)

getArrayDimensions (Array ty bound) = (calculateBound bound):(getArrayDimensions ty)  
getArrayDimensions _ = []

calculateBound (boundLeft, boundRight) = do
  let dimensionLeft = calculateBound' boundLeft
  let dimensionRight = calculateBound' boundRight
  dimensionRight - dimensionLeft 
    where calculateBound' bound = case bound of
            Utils.Type.Fix lenght -> lenght
            Utils.Type.Var _ -> 0

tacGeneratorFunction (FunDec _ signature body) = do
  envTac <-get
  tacs <- tacGeneratorBody body 
  let ident@(PIdent (loc,id)) = getFunNamePident signature
  let tacVoid = TACEntry Nothing VoidOp
  let Function _ retTy = getVarTypeTAC ident envTac
  case retTy of
    Utils.Type.Void -> do
      let entryVoid = TACEntry Nothing ReturnVoid
      return $ tacVoid:attachLabelToFirstElem (Just (id,loc)) (tacs ++ [entryVoid] )
    _ ->  return $ tacVoid:attachLabelToFirstElem (Just (id,loc)) tacs
 

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
  Fun fun _ -> do 
    tacs <- tacGeneratorFunction fun
    pushFunTacs tacs
    return []
  DeclStm (Decl _ declList _ ) -> do
    result <- mapM tacGeneratorDeclaration declList
    return (concat result)
  Block body -> tacGeneratorBody body

tacGeneratorStatement statement = case statement of
  Break (PBreak (pos, _)) _semicolon -> do
    (_,_,_,tree,_,_,_) <- get
    let Just node = findFirstParentsBlockTypeFromPos SequenceBlk tree pos
    label <- newlabel $ getBpEndPos node
    let breakEntry = TACEntry Nothing $ UnconJump label
    return [breakEntry]
  Continue (PContinue (pos,_)) _semicolon -> do
    (_,_,_,tree,_,_,_) <- get
    let Just node = findFirstParentsBlockTypeFromPos SequenceBlk tree pos
    label <- newlabel $ getBpStartPos node
    let continueEntry = TACEntry Nothing $ UnconJump label
    return [continueEntry]
  DoWhile _pdo _while body (SGuard _ guard _) -> do
    labelBegin <- newlabel $ getBodyStartPos body
    labelTrue <- newlabelFALL $ getBodyStartPos body
    labelFalse <- newlabel $ getBodyEndPos body 

    pushLabel $ labelBegin
    res <- tacGeneratorBody body
    
    modify $ addIfSimpleLabels labelBegin labelFalse labelTrue
    (resg,_) <- tacGeneratorExpression RightExp guard
    popSequenceControlLabels

    pushLabel labelFalse
    return (res ++ take (length resg -1 ) resg ) 

  While _pwhile (SGuard _ guard _) body -> do
    labelBegin <- newlabel $ getBodyStartPos body
    labelTrue <- newlabelFALL $ getBodyStartPos body
    labelFalse <- newlabel  $ getBodyEndPos body

    modify $ addIfSimpleLabels labelTrue labelFalse labelBegin
    (resg',_) <- tacGeneratorExpression RightExp guard
    let resg = attachLabelToFirstElem (Just labelBegin) resg'
    popSequenceControlLabels
    
    res <- tacGeneratorBody body
    pushLabel labelFalse
    let gotob = TACEntry Nothing $ UnconJump labelBegin
    return (resg ++ res ++ [gotob]) 

  If (PIf _) (SGuard _ guard _) _then body -> do
    labelTrue <- newlabelFALL $ getBodyStartPos body
    labelFalse <- newlabel $ getBodyEndPos body
    modify $ addIfSimpleLabels labelTrue labelFalse labelFalse

    (resg,_) <- tacGeneratorExpression RightExp guard

    popSequenceControlLabels
    pushLabel labelTrue

    res <- tacGeneratorBody body
    pushLabel labelFalse
    return (resg ++ res) 

  IfElse _ (SGuard _ guard _) _then bodyIf _ bodyElse -> do
    labelTrue <- newlabelFALL $ getBodyStartPos bodyIf 
    labelFalse <- newlabel $ getBodyStartPos bodyElse 
    latestLabel <- newlabel $ getBodyEndPos bodyElse 

    modify $ addIfSimpleLabels labelTrue labelFalse latestLabel
    (resg,_) <- tacGeneratorExpression RightExp guard 
   
    popSequenceControlLabels
    pushLabel labelTrue
    resthen <- tacGeneratorBody bodyIf
    let goto = TACEntry Nothing $ UnconJump latestLabel

    pushLabel labelFalse
    reselse <- tacGeneratorBody bodyElse
    pushLabel latestLabel
 
    return (resg ++ resthen ++ goto:reselse)
  StExp (EFun funidentifier _ passedparams _) _semicolon-> do
    (tacs, _) <- tacGeneratorFunctionCall LeftExp funidentifier passedparams
    return tacs
  StExp exp _semicolon -> do
    (tacEntry, _) <- tacGeneratorExpression LeftExp exp
    return $ reverse tacEntry
  RetVal _ret exp _semicolon -> do
    (tacs, temp) <- tacGeneratorExpression RightExp exp
    let tacEntry = TACEntry Nothing $ ReturnValue temp
    return $ tacEntry:tacs
  RetVoid _ret _semicolon -> return [TACEntry Nothing ReturnVoid] 

attachLabelToFirstElem _ [] = []
attachLabelToFirstElem (Just label) ((TACEntry Nothing _e):xs) = (TACEntry (Just label) _e):xs
attachLabelToFirstElem (Just label) ((TACEntry (Just _l) _e):xs) = (TACEntry (Just label) VoidOp):(TACEntry (Just _l) _e):xs
attachLabelToFirstElem Nothing xs = xs

tacGeneratorExpression expType exp = case exp of
    EAss e1 assign e2 -> tacGeneratorAssignment e1 assign e2
    Eplus e1 (PEplus (loc,_)) e2 -> tacGeneratorExpression' expType e1 Plus e2 loc
    Emod e1 (PEmod (loc,_)) e2 -> tacGeneratorExpression' expType e1 Modul e2 loc
    Eminus e1 (PEminus (loc,_)) e2 -> tacGeneratorExpression' expType e1 Minus e2 loc
    Ediv e1 (PEdiv (loc,_)) e2 ->  tacGeneratorExpression' expType e1 Div e2 loc
    Etimes e1 (PEtimes (loc,_)) e2 -> tacGeneratorExpression' expType e1 Times e2 loc
    InnerExp _ e _ -> tacGeneratorExpression expType e
    Elthen e1 (PElthen (loc,_)) e2 -> tacCheckerBinaryBoolean expType loc e1 ThreeAddressCode.TAC.LT e2
    Elor e1 (PElor (loc,_)) e2 -> tacCheckerBinaryBoolean expType loc e1 ThreeAddressCode.TAC.OR e2
    Eland e1 (PEland (loc,_)) e2 -> tacCheckerBinaryBoolean expType loc e1 ThreeAddressCode.TAC.AND e2
    Eneq e1 (PEneq (loc,_)) e2 -> tacCheckerBinaryBoolean expType loc e1 ThreeAddressCode.TAC.NEQ e2
    Eeq e1 (PEeq (loc,_)) e2 -> tacCheckerBinaryBoolean expType loc e1 ThreeAddressCode.TAC.EQ e2
    Egrthen e1 (PEgrthen (loc,_)) e2 -> tacCheckerBinaryBoolean expType loc e1 ThreeAddressCode.TAC.GT e2
    Ele e1 (PEle (loc,_)) e2 -> tacCheckerBinaryBoolean expType loc e1 ThreeAddressCode.TAC.LTE e2
    Ege e1 (PEge (loc,_)) e2 -> tacCheckerBinaryBoolean expType loc e1 ThreeAddressCode.TAC.GTE e2
    Epreop (Negation (PNeg (loc,_))) e1 -> tacCheckerUnaryBoolean expType loc e1
    Epreop (Indirection (PEtimes (loc,_))) e1 -> tacGeneratorPointer expType loc e1 
    Epreop (Address (PDef (loc,_) )) e1 -> tacGeneratorAddress expType loc e1
    Earray expIdentifier arDeclaration -> tacGeneratorArrayIndexing expIdentifier arDeclaration
    EFun funidentifier _ passedparams _ -> tacGeneratorFunctionCall expType funidentifier passedparams
    Evar identifier -> tacGeneratorVariable expType identifier
    Econst (Estring (PString desc)) ->tacGeneratorConstant desc String
    Econst (Eint (PInteger desc)) -> tacGeneratorConstant desc Int
    Econst (Efloat (PDouble desc)) -> tacGeneratorConstant desc Real
    Econst (Echar (PChar desc)) -> tacGeneratorConstant desc Char
    Econst (ETrue (PTrue desc)) -> tacGeneratorConstant desc Bool
    Econst (EFalse (PFalse desc)) -> tacGeneratorConstant desc Bool


tacGeneratorVariable expType identifier@(PIdent (_, id)) = do
  tacEnv <- get
  let Variable loc ty = getVarTypeTAC identifier tacEnv
  let varTemp =  Temp ThreeAddressCode.TAC.Var id loc ty
  case ty of 
    Reference tyRef -> case expType of
      RightExp ->  do
        idVal <- newtemp
        let valTemp =  Temp ThreeAddressCode.TAC.Var idVal loc tyRef
        let entryRef = TACEntry Nothing $ ReferenceRight valTemp varTemp
        return ([entryRef], valTemp)
      LeftExp ->  return ([], varTemp)
    _ ->  return ([], varTemp)


tacGeneratorAddress _expType loc e1 = do
  (tacs, temp@(Temp mod id l ty)) <- tacGeneratorExpression LeftExp e1
  case ty of 
    Reference ty -> return (tacs, Temp mod id l ty)
    _ -> do
      defId <- newtemp
      let defTemp = Temp ThreeAddressCode.TAC.Var defId loc ty
      let defTac = TACEntry Nothing $  DeferenceRight defTemp temp
      return (defTac:tacs,defTemp)

tacGeneratorPointer expType loc e1 = do
  (tacs, temp@(Temp mod id l ty)) <- tacGeneratorExpression expType e1
  case ty of 
    Reference ty -> return (tacs, Temp mod id l ty)
    _ -> do
      refId <- newtemp
      let refTemp = Temp ThreeAddressCode.TAC.Var refId loc ty
      let refTac = TACEntry Nothing $  ReferenceRight refTemp temp
      return (refTac:tacs,refTemp)

tacGeneratorExpression' exptype e1 op e2 loc = do
  (tac1,temp1@(Temp _ _ _ ty1)) <- tacGeneratorExpression exptype e1
  (tac2,temp2@(Temp _ _ _ ty2)) <- tacGeneratorExpression exptype e2
  idTemp <- newtemp
  let temp = Temp ThreeAddressCode.TAC.Var idTemp loc (supTac ty1 ty2)
  let newEntry = TACEntry Nothing (Binary temp temp1 op temp2)
  let tac = newEntry:(tac1 ++ tac2)
  return (tac, temp)

tacGeneratorConstant (loc, id) ty = return ([], Temp ThreeAddressCode.TAC.Fix id loc ty)

genLazyTacAND vtype loc e1 _op e2 _ltrue lfalse = 
  if (isLabelFALL lfalse)
    then do
        l1true <- newlabelFALL loc
        l1false <- newlabel loc

        modify $ addIfSimpleLabels l1true l1false l1false
        (tac1,_) <- tacGeneratorExpression vtype e1

        modify $ addIfSimpleLabels l1true l1false l1false

        label <- popLabel
        (tac2',temp2') <- tacGeneratorExpression vtype e2
        (tac2, temp2) <- return (attachLabelToFirstElem label tac2', temp2')

        let tacs = tac1 ++ tac2
        if (isLabelFALL lfalse)
          then do
              pushLabel l1false
              return (tacs, temp2)
          else return (tacs, temp2)
    else do
        let l1false = lfalse
        l1true <- newlabelFALL loc
        modify $ addIfSimpleLabels l1true l1false l1false
        (tac1,_) <- tacGeneratorExpression vtype e1

        modify $ addIfSimpleLabels l1true l1false l1false
        label <- popLabel
        (tac2',temp2') <- tacGeneratorExpression vtype e2
        (tac2, temp2) <- return (attachLabelToFirstElem label tac2', temp2')

        let tacs = tac1 ++ tac2
        if (isLabelFALL lfalse)
          then do
            pushLabel l1false
            return (tacs, temp2)
          else return (tacs, temp2)

genLazyTacOR vtype loc e1 _op e2 ltrue lfalse = 
  if not (isLabelFALL ltrue)
    then
      do
        l1false <- newlabelFALL loc
        l1true <- return ltrue
        modify $ addIfSimpleLabels l1true l1false l1false
        (tac1,_) <- tacGeneratorExpression vtype e1

        _l2false <- return lfalse
        _l2true <- return ltrue
        modify $ addIfSimpleLabels l1true l1false l1false
        label <- popLabel
        (tac2',temp2') <- tacGeneratorExpression vtype e2
        (tac2, temp2) <- return (attachLabelToFirstElem label tac2', temp2')

        let tacs = tac1 ++ tac2
        if not (isLabelFALL ltrue)
          then return (tacs, temp2)
          else do
              pushLabel l1true
              return (tacs, temp2)
    else
      do
        l1false <- newlabelFALL loc
        l1true <- newlabel loc
        modify $ addIfSimpleLabels l1true l1false l1false
        (tac1,_) <- tacGeneratorExpression vtype e1

        l2false <- return lfalse
        l2true <- return ltrue
        modify $ addIfSimpleLabels l2true l2false l1false
        label <- popLabel
        (tac2',temp2') <- tacGeneratorExpression vtype e2
        (tac2, temp2) <- return (attachLabelToFirstElem label tac2', temp2')

        let tacs = tac1 ++ tac2
        if not (isLabelFALL ltrue)
          then return (tacs, temp2)
          else do
              pushLabel l1true
              return (tacs, temp2)

genLazyTacREL vtype _ e1 op e2 ltrue lfalse = do
  (tac1,temp1) <- tacGeneratorExpression vtype e1
  (tac2,temp2) <- tacGeneratorExpression vtype e2
  case (isLabelFALL ltrue, isLabelFALL lfalse) of
    (True,True) -> return (tac1 ++ tac2, temp1)
    (_, True) -> do
      let aa = TACEntry Nothing (RelCondJump temp1 op temp2 ltrue)
      return (tac1 ++ tac2 ++ [aa], temp2)
    (True, _) -> do
      let (ope, t1, t2) = notRel op temp1 temp2
      let aa = TACEntry Nothing (RelCondJump t1 ope t2 lfalse) 
      return (tac1 ++ tac2 ++ [aa], temp2)
    (_,_) -> do
      let aa = TACEntry Nothing (RelCondJump temp1 op temp2 ltrue)
      let bb = TACEntry Nothing (UnconJump lfalse) 
      return (tac1 ++ tac2 ++ (aa:bb:[]) , temp2)


tacCheckerBinaryBoolean vtype loc e1 op e2 = do
  labels <- popSequenceControlLabels
  setSequenceControlLabels labels
  case labels of
    (SequenceLazyEvalLabels ltrue lfalse _) -> case op of
      AND -> genLazyTacAND vtype loc e1 op e2 ltrue lfalse
      OR -> genLazyTacOR vtype loc e1 op e2 ltrue lfalse
      _ -> genLazyTacREL vtype loc e1 op e2 ltrue lfalse

tacCheckerUnaryBoolean expType _loc e1 = do
  labels <- popSequenceControlLabels
  setSequenceControlLabels labels
  case labels of
      (SequenceLazyEvalLabels ltrue lfalse lbreak) -> do
        modify $ addIfSimpleLabels lfalse ltrue lbreak
        (tac1,temp1) <- tacGeneratorExpression expType e1
        return (tac1, temp1)


notRel ThreeAddressCode.TAC.LT t1 t2 = (GTE, t1, t2)
notRel ThreeAddressCode.TAC.LTE t1 t2 = (ThreeAddressCode.TAC.GT, t1, t2)
notRel ThreeAddressCode.TAC.GT t1 t2 = (LTE, t1, t2)
notRel ThreeAddressCode.TAC.GTE t1 t2 = (ThreeAddressCode.TAC.LT, t1, t2)
notRel ThreeAddressCode.TAC.EQ t1 t2 = (NEQ, t1, t2)
-- non dovrei mai arrivare a chiamare i due casi sotto, se ci arrivo c'e qualcosa di sbagliato
-- notRel ThreeAddressCode.TAC.AND t1 t2 = (OR, t1, t2)
-- notRel ThreeAddressCode.TAC.OR t1 t2 = (AND, t1, t2)

tacGeneratorFunctionCall expType identifier@(PIdent (_, id)) paramsPassed = do
  tacEnv <- get
  tacParameters <- mapM (tacGeneratorFunctionParameter expType) paramsPassed
  let tacExp = concatMap fst tacParameters
  let tempPars =  map snd tacParameters
  let (loc,funParams) = getFunctionParams identifier tempPars tacEnv 
  let Function _ retTy = getVarTypeTAC identifier tacEnv
  let parameterLenght = length paramsPassed
  tacPar <- mapM tacGenerationTempParameter $ zip funParams tempPars
  let funTemp = Temp ThreeAddressCode.TAC.Var id loc retTy
  let tacsTemp = reverse $ map (tacGenerationEntryParameter.snd) tacPar
  let tacs = reverse $ tacsTemp ++ concatMap fst tacPar ++ tacExp 
  case retTy of
    Utils.Type.Void -> do
      let tacEntry = TACEntry Nothing $ CallProc funTemp parameterLenght
      return (tacs ++ [ tacEntry],funTemp)
    _ -> do
      idResFun <- newtemp
      let idResTemp = Temp ThreeAddressCode.TAC.Var idResFun loc retTy
      let tacEntry = TACEntry Nothing $ CallFun idResTemp funTemp parameterLenght
      return (tacs ++ [tacEntry], idResTemp)


tacGeneratorFunctionParameter expType (PassedPar exp) =
  case exp of
    Evar identifier@(PIdent (_, id)) -> do
      tacEnv <- get
      let Variable loc ty = getVarTypeTAC identifier tacEnv
      let varTemp =  Temp ThreeAddressCode.TAC.Var id loc ty 
      return ([], varTemp)
    _ -> tacGeneratorExpression expType exp


tacGenerationEntryParameter temp = TACEntry Nothing $ SetParam temp

tacGenerationTempParameter ((Variable locMode tyPar),temp@(Temp _ _ _ tyTemp)) = 
    case (tyPar, tyTemp) of
      (Reference _, Reference _)  -> return ([], temp)
      (_, Reference tyFound) -> do
        idVal <- newtemp 
        let valTemp = Temp ThreeAddressCode.TAC.Var idVal locMode tyFound
        let valEntry = TACEntry Nothing $ ReferenceRight valTemp temp
        return ([valEntry],valTemp )
      (Reference tyFound, _) -> do
        idRef <- newtemp
        let refTemp = Temp ThreeAddressCode.TAC.Var idRef locMode tyFound
        let refEntry = TACEntry Nothing $ DeferenceRight refTemp temp
        return ([refEntry], refTemp)
      _ -> return ([], temp)

tacGeneratorArrayIndexing var@(Evar identifier@(PIdent (_, id))) arrayDecl = do
  tacEnv <- get
  let Variable loc ty = getVarTypeTAC identifier tacEnv
  (tacsAdd, temp) <- tacGeneratorArrayIndexing' var arrayDecl
  idArVal <- newtemp
  let tempArrayVal = Temp ThreeAddressCode.TAC.Var idArVal loc ty 
  let tempId = Temp ThreeAddressCode.TAC.Var id loc ty
  let tacEntry = TACEntry Nothing $ IndexRight tempArrayVal tempId temp
  return (tacEntry:tacsAdd, tempArrayVal)

tacGeneratorArrayIndexing' (Evar identifier) arrayDecl = do
  tacEnv <- get
  let var = getVarTypeTAC identifier tacEnv
  (tacsPos, temps) <- tacGenerationArrayPosition arrayDecl
  (tacsAdd, temp) <- tacGenerationArrayPosAdd var temps
  return (tacsAdd ++ tacsPos, temp)

tacGeneratorAssignment leftExp assign rightExp = do
  (tacRight,tempRight) <- tacGeneratorExpression RightExp rightExp
  (tacLeft, operation, (Temp _ _ locLeft tyLeft)) <- tacGeneratorLeftExpression leftExp assign tempRight
  idTemp <- newtemp
  let tacs = tacRight ++ tacLeft 
  let temp = Temp ThreeAddressCode.TAC.Var idTemp locLeft tyLeft
  case operation of 
    Nullary temp1 temp2 -> case assign of
      AssgnEq (PAssignmEq _) -> return (TACEntry Nothing operation:tacs, temp)
      AssgnPlEq (PAssignmPlus _) -> do
        let entryAdd = TACEntry Nothing $ Binary temp1 temp1 Plus temp2
        return (entryAdd:tacs, temp)
    IndexLeft variable@(Temp _ _ _ ty) index tempRight -> case assign of 
      AssgnEq (PAssignmEq _) -> return (TACEntry Nothing operation:tacs, temp)
      AssgnPlEq (PAssignmPlus (loc,_)) -> do
        idTempAdd <- newtemp
        let tempAdd = Temp ThreeAddressCode.TAC.Var idTempAdd loc ty
        let entryArrayValue = TACEntry Nothing $ IndexRight temp variable index
        let entryadd = TACEntry Nothing $ Binary tempAdd temp Plus tempRight 
        let entryFinal = TACEntry Nothing $ IndexLeft variable index tempAdd
        return ((entryFinal:entryadd:entryArrayValue:[]) ++ tacs, temp)
    ReferenceLeft tempLeft@(Temp _ _ _ ty) tempRight -> case assign of
      AssgnEq (PAssignmEq _) -> return (TACEntry Nothing operation:tacs, temp)
      AssgnPlEq (PAssignmPlus (loc,_)) -> do
        idRefValue <- newtemp
        let tempRefValue = Temp ThreeAddressCode.TAC.Var idRefValue loc ty
        let entryPointerValue = TACEntry Nothing $ ReferenceRight tempRefValue tempLeft
        idTempAdd <- newtemp
        let tempAdd = Temp ThreeAddressCode.TAC.Var idTempAdd loc ty
        let entryadd = TACEntry Nothing $ Binary tempAdd tempRefValue Plus tempRight 
        let entryPointerAdd = TACEntry Nothing $ ReferenceLeft tempLeft tempAdd
        return ((entryPointerAdd:entryadd:entryPointerValue:[]) ++ tacs, temp)

tacGeneratorLeftExpression leftExp _assgn tempRight = case leftExp of
  Evar identifier@(PIdent (_, id)) -> do
    tacEnv <- get
    let Variable loc ty = getVarTypeTAC identifier tacEnv
    let varTemp =  Temp ThreeAddressCode.TAC.Var id loc ty
    let Variable _loc ty = getVarTypeTAC identifier tacEnv
    case ty of
      Reference _ty -> return ([], ReferenceLeft varTemp tempRight, varTemp)
      _ ->  return ([], Nullary varTemp tempRight, varTemp)
  Earray expIdentifier arDeclaration -> do
    (_, tempLeft) <- tacGeneratorExpression LeftExp expIdentifier
    (tacsAdd, temp) <- tacGeneratorArrayIndexing' expIdentifier arDeclaration
    return (tacsAdd, IndexLeft tempLeft temp tempRight, tempLeft)
  Epreop (Indirection _) e1 -> do
    (tacs, tempLeft) <- tacGeneratorExpression LeftExp e1
    return (tacs, ReferenceLeft tempLeft tempRight, tempLeft)

tacGenerationArrayPosition (ArrayInit _ expressions _ ) = tacGenerationArrayPosition' expressions
  where
    tacGenerationArrayPosition' [ExprDec x] = do
      (tacExp, tempExp) <- tacGeneratorExpression RightExp x 
      return (tacExp, [tempExp])
    tacGenerationArrayPosition' (ExprDec x:xs) = do
      (tacExp, tempExp) <- tacGeneratorExpression RightExp x 
      (tacOther, tempOthers) <- tacGenerationArrayPosition' xs
      return ((tacExp ++ tacOther), tempExp:tempOthers)

tacGenerationArrayPosAdd (Variable loc ty) temps = do
  (tacs, temp) <- tacGenerationArrayPosAdd' (tail (reverse (getArrayDimensions ty))) (reverse temps)
  idSize <- newtemp
  let size = sizeof ty
  let tempSize = Temp ThreeAddressCode.TAC.Fix (show size) loc Int 
  let tempResult =  Temp ThreeAddressCode.TAC.Var idSize loc Int 
  let sizeEntry =  TACEntry Nothing $ Binary tempResult temp Times tempSize
  return (sizeEntry:tacs,tempResult)
    where
      tacGenerationArrayPosAdd' _ [x] = return ([], x)
      tacGenerationArrayPosAdd' (x:xs) (y@(Temp _ _ loc _):ys) = do
        (tacs, temp) <- tacGenerationArrayPosAdd' xs ys
        idMultTemp <- newtemp
        idAddTemp <- newtemp
        let tempLength = Temp ThreeAddressCode.TAC.Fix (show x) loc Int
        let tempMult = Temp ThreeAddressCode.TAC.Var idMultTemp loc Int
        let tempAdd = Temp ThreeAddressCode.TAC.Var idAddTemp loc Int
        let mulEntry = TACEntry Nothing $ Binary tempMult temp Times tempLength
        let addEntry =  TACEntry Nothing $ Binary tempAdd y Plus tempMult
        return (addEntry:mulEntry:tacs,tempAdd)
