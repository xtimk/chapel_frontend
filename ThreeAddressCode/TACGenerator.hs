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
    AssgmDec ids assgn exp -> tacGeneratorDeclExpression ids assgn exp
    AssgmTypeDec ids _ _ assgn exp -> tacGeneratorDeclExpression ids assgn exp
    NoAssgmArrayDec  {} -> return []

tacGeneratorDeclExpression ids assgn exp = do
  temps <- mapM tacGeneratorIdentifierTemp ids
  tacGeneratorDeclExpression' assgn [] temps exp

tacGeneratorDeclExpression' assgn lengths temps decExp = 
  case decExp of 
    ExprDecArray (ArrayInit _ expressions _ ) -> tacGeneratorDeclExpression'' assgn (0:lengths) temps expressions
    ExprDec exp -> do
      let firstTemp@(Temp _ _ loc ty) = head temps
      (tacEntry, temp, labelExit) <- tacGeneratorRightExpression' loc (getBasicType ty)
      if null lengths
        then do
          resFirst <- tacGeneratorIdentifier labelExit temp firstTemp
          res <- mapM (tacGeneratorIdentifier Nothing temp) (tail temps)
          return $ reverse tacEntry ++ [resFirst] ++ res
        else do
          resFirst <- tacGeneratorArrayIdentifier labelExit temp  (getExpPos exp) lengths firstTemp
          res <- mapM (tacGeneratorArrayIdentifier Nothing temp (getExpPos exp) lengths ) (tail temps)
          return $ reverse tacEntry ++ [resFirst] ++ res
        where
          tacGeneratorRightExpression' _ ty = case ty of 
            Bool -> tacGeneratorBooleanStatement (getAssignOpPos assgn) exp
            _ -> do
              (tacsRight, tempRight) <- tacGeneratorExpression RightExp exp
              return (tacsRight, tempRight, Nothing)


tacGeneratorDeclExpression'' _ _ _ [] = return []
tacGeneratorDeclExpression'' assgn lengths@(x:xs) ids (y:ys) = do
  res1 <- tacGeneratorDeclExpression'' assgn ((x+1):xs) ids ys
  res2 <- tacGeneratorDeclExpression' assgn lengths ids y
  return (res1 ++ res2)
  
tacGeneratorIdentifierTemp id@(PIdent (_, identifier)) = do
  tacEnv <- get
  let Checker.SymbolTable.Variable loc ty = getVarTypeTAC id tacEnv
  let temp = Temp ThreeAddressCode.TAC.Variable identifier loc ty
  return temp

tacGeneratorIdentifier labelExit tempRight tempLeft =
  return $ TACEntry labelExit  $ Nullary tempLeft tempRight

tacGeneratorArrayIdentifier labelExit temp _ lenghts tempLeft@(Temp _ idVar locVar ty) = do
  tacEnv <- get
  let Checker.SymbolTable.Variable loc ty = getVarTypeTACDirect locVar idVar tacEnv
  let arrayPos = arrayCalculatePosition ty lenghts
  let temp1 = Temp ThreeAddressCode.TAC.Variable idVar loc ty
  let temp2 = Temp ThreeAddressCode.TAC.Fixed (show arrayPos) loc Int
  return $ TACEntry labelExit $ IndexLeft temp1 temp2 temp

arrayCalculatePosition ty lengths = sizeof ty * arrayCalculatePosition' (reverse (getArrayDimensions ty)) lengths
  where
    arrayCalculatePosition' _ [x] =  x 
    arrayCalculatePosition' (y:ys) (x:xs) = x + y * arrayCalculatePosition' ys xs

getArrayDimensions (Array ty bound) = calculateBound bound:getArrayDimensions ty  
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
      return $ tacVoid:attachLabelToFirstElem (Just (id,loc, FunLb)) (tacs ++ [entryVoid] )
    _ ->  return $ tacVoid:attachLabelToFirstElem (Just (id,loc, FunLb)) tacs

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
    label <- newlabel (getBpEndPos node) BreakLb
    let breakEntry = TACEntry Nothing $ UnconJump label
    return [breakEntry]
  Continue (PContinue (pos,_)) _semicolon -> do
    (_,_,_,tree,_,_,_) <- get
    let Just node = findFirstParentsBlockTypeFromPos SequenceBlk tree pos
    label <- newlabel (getBpStartPos node) ContinueLb
    let continueEntry = TACEntry Nothing $ UnconJump label
    return [continueEntry]
  DoWhile (Pdo (loc,_)) _while body (SGuard _ guard _) -> do
    labelJump <- newlabel loc VoidLb
    let entryJump = TACEntry Nothing $ UnconJump labelJump

    labelBegin <- newlabel (getBodyStartPos body) WhileLb
    labelTrue <- newlabelFALL (getBodyStartPos body) WhileLb
    labelFalse <- newlabel  (getBodyEndPos body) WhileExitLb

    modify $ addIfSimpleLabels labelTrue labelFalse labelBegin
    (resg',_) <- tacGeneratorExpression BooleanExp guard
    let resg = entryJump:attachLabelToFirstElem (Just labelBegin) resg'
    popSequenceControlLabels
    
    res' <- tacGeneratorBody body
    let res = attachLabelToFirstElem (Just labelJump) res' 
    pushLabel labelFalse
    let gotob = TACEntry Nothing $ UnconJump labelBegin
    return (resg ++ res ++ [gotob]) 

  While _pwhile (SGuard _ guard _) body -> do
    labelBegin <- newlabel (getBodyStartPos body) WhileLb
    labelTrue <- newlabelFALL (getBodyStartPos body) WhileLb
    labelFalse <- newlabel  (getBodyEndPos body) WhileExitLb

    modify $ addIfSimpleLabels labelTrue labelFalse labelBegin
    (resg',_) <- tacGeneratorExpression BooleanExp guard
    let resg = attachLabelToFirstElem (Just labelBegin) resg'
    popSequenceControlLabels
    
    res <- tacGeneratorBody body
    pushLabel labelFalse
    let gotob = TACEntry Nothing $ UnconJump labelBegin
    return (resg ++ res ++ [gotob]) 

  If (PIf _) (SGuard _ guard _) _then body -> do
    labelTrue <- newlabelFALL (getBodyStartPos body) IfLb
    labelFalse <- newlabel (getBodyEndPos body) IfLb
    modify $ addIfSimpleLabels labelTrue labelFalse labelFalse

    (resg,_) <- tacGeneratorExpression BooleanExp guard

    popSequenceControlLabels
    pushLabel labelTrue

    res <- tacGeneratorBody body
    pushLabel labelFalse
    return (resg ++ res) 

  IfElse _ (SGuard _ guard _) _then bodyIf _ bodyElse -> do
    labelTrue <- newlabelFALL (getBodyStartPos bodyIf) IfThenLb
    labelFalse <- newlabel (getBodyStartPos bodyElse) IfThenLb
    latestLabel <- newlabel (getBodyEndPos bodyElse) ElseLb

    modify $ addIfSimpleLabels labelTrue labelFalse latestLabel
    (resg,_) <- tacGeneratorExpression BooleanExp guard 
   
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

attachLabelToFirstElem (Just label) [] = [TACEntry (Just label) VoidOp]
attachLabelToFirstElem (Just label) ((TACEntry Nothing _e):xs) = TACEntry (Just label) _e:xs
attachLabelToFirstElem (Just label) ((TACEntry (Just _l) _e):xs) = TACEntry (Just label) VoidOp:TACEntry (Just _l) _e:xs
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
    Econst constant@(ETrue _) -> tacCheckExpressionConstantBoolean expType constant
    Econst constant@(EFalse _) ->  do
      tacCheckerReverBooleanLabel
      tacCheckExpressionConstantBoolean expType constant
    Econst constant -> tacGeneratorConstant constant

tacCheckExpressionConstantBoolean expType constant = do
  res@(_,temp) <- tacGeneratorConstant constant 
  case expType of
    BooleanExp -> tacGeneratorBooleanConstant temp
    RightExp -> return res

tacGeneratorConstant constant = case constant of
  Estring (PString (loc,id)) -> return ([], Temp ThreeAddressCode.TAC.Fixed id loc String)
  Eint (PInteger (loc,id)) -> return ([], Temp ThreeAddressCode.TAC.Fixed id loc Int)
  Efloat (PDouble (loc,id)) -> return ([], Temp ThreeAddressCode.TAC.Fixed id loc Real)
  Echar (PChar (loc,id)) -> return ([], Temp ThreeAddressCode.TAC.Fixed id loc Char)
  ETrue (PTrue (loc,id))-> return ([], Temp ThreeAddressCode.TAC.Fixed id loc Bool)
  EFalse (PFalse (loc,id)) -> return ([], Temp ThreeAddressCode.TAC.Fixed id loc Bool)
    
tacGeneratorBooleanStatement loc rightExp = do
  labelTrue <- newlabelFALL loc TrueBoolStmLb
  labelFalse <- newlabel loc FalseBoolStmLb
  labelExit <- newlabel loc ExitBoolStmLb
  modify $ addIfSimpleLabels labelTrue labelFalse labelFalse
  (resg,temp@(Temp mode  _ _ ty)) <- tacGeneratorExpression BooleanExp rightExp 
  popSequenceControlLabels
  labelTrue' <-  popLabel
  case (mode,ty) of 
    (Temporary, Bool) -> do
      idTempRight <- newtemp
      let tempTrue = Temp Fixed "true" (-1,-1) Bool
      let tempFalse = Temp Fixed "false" (-1,-1) Bool
      let tempRight = Temp Temporary idTempRight loc Bool
      let tacTrue = TACEntry labelTrue' $ Nullary tempRight tempTrue
      let tacGoToExit = TACEntry Nothing $ UnconJump labelExit
      let tacFalse = TACEntry (Just labelFalse) $ Nullary tempRight tempFalse
      return (tacFalse:tacGoToExit:tacTrue: reverse resg,tempRight, Just labelExit)
    (ThreeAddressCode.TAC.Variable, Bool) -> return ([], temp, Nothing)
    (Fixed, Bool) -> return ([], temp, Nothing)
    _ -> return (resg, temp, Nothing)
           


tacGeneratorVariable expType identifier@(PIdent (_, id)) = do
  tacEnv <- get
  let Checker.SymbolTable.Variable loc ty = getVarTypeTAC identifier tacEnv
  let varTemp =  Temp ThreeAddressCode.TAC.Variable id loc ty
  case ty of 
    Reference tyRef -> case expType of
      RightExp -> createReferenceEntry loc tyRef varTemp
      LeftExp ->  return ([], varTemp)
      BooleanExp -> do
        (tacs, valTemp) <- createReferenceEntry loc tyRef varTemp  
        (tacsBool, boolTemp) <- tacGeneratorBooleanVariable valTemp
        return (tacs ++ tacsBool,boolTemp)
    _ -> case expType of 
      BooleanExp -> tacGeneratorBooleanVariable varTemp
      _ -> return ([], varTemp)
    where 
      createReferenceEntry loc tyRef varTemp = do
        idVal <- newtemp
        let valTemp =  Temp ThreeAddressCode.TAC.Temporary idVal loc tyRef
        let entryRef = TACEntry Nothing $ ReferenceRight valTemp varTemp
        return ([entryRef], valTemp)


tacGeneratorAddress _expType loc e1 = do
  (tacs, temp@(Temp mod id l ty)) <- tacGeneratorExpression LeftExp e1
  case ty of 
    Reference ty -> return (tacs, Temp mod id l ty)
    _ -> do
      defId <- newtemp
      let defTemp = Temp ThreeAddressCode.TAC.Temporary defId loc ty
      let defTac = TACEntry Nothing $  DeferenceRight defTemp temp
      return (defTac:tacs,defTemp)

tacGeneratorPointer expType loc e1 = do
  (tacs, temp@(Temp mod id l ty)) <- tacGeneratorExpression expType e1
  case ty of 
    Reference ty -> return (tacs, Temp mod id l ty)
    _ -> do
      refId <- newtemp
      let refTemp = Temp ThreeAddressCode.TAC.Temporary refId loc ty
      let refTac = TACEntry Nothing $  ReferenceRight refTemp temp
      return (refTac:tacs,refTemp)

tacGeneratorExpression' exptype e1 op e2 loc = do
  (tac1,temp1@(Temp _ _ _ ty1)) <- tacGeneratorExpression exptype e1
  (tac2,temp2@(Temp _ _ _ ty2)) <- tacGeneratorExpression exptype e2
  idTemp <- newtemp
  let temp = Temp ThreeAddressCode.TAC.Temporary idTemp loc (supTac ty1 ty2)
  let newEntry = TACEntry Nothing (Binary temp temp1 op temp2)
  let tac = newEntry:(tac1 ++ tac2)
  return (tac, temp)



tacCheckerBinaryBoolean vtype loc e1 op e2 = do
  let fakeTemp = Temp Temporary "fake" (-1,-1) Bool
  labels <- popSequenceControlLabels
  setSequenceControlLabels labels
  case labels of
    (SequenceLazyEvalLabels ltrue lfalse _) -> case op of
      AND -> do
        (tacs, _) <- genLazyTacAND BooleanExp loc e1 op e2 ltrue lfalse
        return (tacs,fakeTemp)
      OR -> do
        (tacs, _) <- genLazyTacOR BooleanExp loc e1 op e2 ltrue lfalse
        return (tacs,fakeTemp)
      _ -> do
        (tacs1, temp1, label1) <- tacGeneratorBooleanStatement (getExpPos e1) e1
        (tacs2, temp2, label2) <- tacGeneratorBooleanStatement (getExpPos e2) e2
        let tacs = reverse tacs1 ++ attachLabelToFirstElem label1 (reverse tacs2)
        (tacRel, _temp) <- genLazyTacREL label2 temp1 op temp2 ltrue lfalse
        return (tacs ++ tacRel, fakeTemp)
    
genLazyTacAND vtype loc e1 _op e2 _ltrue lfalse = 
  if isLabelFALL lfalse
    then do
        l1true <- newlabelFALL loc VoidLb -- TrueBoolStmLb
        l1false <- newlabel loc VoidLb --FalseBoolStmLb

        modify $ addIfSimpleLabels l1true l1false l1false
        (tac1,_) <- tacGeneratorExpression vtype e1

        modify $ addIfSimpleLabels l1true l1false l1false

        label <- popLabel
        (tac2',temp2') <- tacGeneratorExpression vtype e2
        (tac2, temp2) <- return (attachLabelToFirstElem label tac2', temp2')

        let tacs = tac1 ++ tac2
        if isLabelFALL lfalse
          then do
              pushLabel l1false
              return (tacs, temp2)
          else return (tacs, temp2)
    else do
        let l1false = lfalse
        l1true <- newlabelFALL loc VoidLb -- TrueBoolStmLb
        modify $ addIfSimpleLabels l1true l1false l1false
        (tac1,_) <- tacGeneratorExpression vtype e1

        modify $ addIfSimpleLabels l1true l1false l1false
        label <- popLabel
        (tac2',temp2') <- tacGeneratorExpression vtype e2
        (tac2, temp2) <- return (attachLabelToFirstElem label tac2', temp2')

        let tacs = tac1 ++ tac2
        if isLabelFALL lfalse
          then do
            pushLabel l1false
            return (tacs, temp2)
          else return (tacs, temp2)

genLazyTacOR vtype loc e1 _op e2 ltrue lfalse = 
  if not (isLabelFALL ltrue)
    then
      do
        l1false <- newlabelFALL loc VoidLb --FalseBoolStmLb
        let l1true = ltrue
        modify $ addIfSimpleLabels l1true l1false l1false
        (tac1,_) <- tacGeneratorExpression vtype e1

        let _l2false = lfalse
        let _l2true = ltrue
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
        l1false <- newlabelFALL loc VoidLb --FalseBoolStmLb
        l1true <- newlabel loc TrueBoolStmLb
        modify $ addIfSimpleLabels l1true l1false l1false
        (tac1,_) <- tacGeneratorExpression vtype e1

        let l2false = lfalse
        let l2true = ltrue
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


tacGeneratorBooleanConstant constTemp = do
  labels@(SequenceLazyEvalLabels ltrue lfalse _) <- popSequenceControlLabels
  setSequenceControlLabels labels
  case (isLabelFALL ltrue, isLabelFALL lfalse) of
    (True,True) -> return ([], constTemp)
    (_, True) -> return ([TACEntry Nothing $ UnconJump ltrue], constTemp)
    (True, _) -> return ([], constTemp)
    (_,_) -> return ([TACEntry Nothing $ UnconJump ltrue], constTemp)

tacGeneratorBooleanVariable varTemp = do
  labels@(SequenceLazyEvalLabels ltrue lfalse _) <- popSequenceControlLabels
  setSequenceControlLabels labels
  case (isLabelFALL ltrue, isLabelFALL lfalse) of
    (True,True) -> return ([], varTemp) 
    (_, True) -> return ([TACEntry Nothing $ BoolTrueCondJump varTemp ltrue], varTemp)
    (True, _) -> return ([TACEntry Nothing $ BoolFalseCondJump varTemp lfalse], varTemp)
    (_,_) -> return ([TACEntry Nothing (BoolTrueCondJump varTemp ltrue),TACEntry Nothing (UnconJump lfalse)], varTemp)

genLazyTacREL label temp1@(Temp mode1 _ _ ty) op temp2 ltrue lfalse =
  case (isLabelFALL ltrue, isLabelFALL lfalse) of
    (True,True) -> return ([], temp1)
    (_, True) -> do
          let aa = TACEntry label (RelCondJump temp1 op temp2 ltrue)
          return ([aa], temp2)
    (True, _) -> do
          let ope = notRel op
          let aa = TACEntry label (RelCondJump temp1 ope temp2 lfalse) 
          return ([aa], temp2)
    (_,_) -> do 
          let aa = TACEntry label (RelCondJump temp1 op temp2 ltrue)
          let bb = TACEntry Nothing (UnconJump lfalse) 
          return ([aa,bb] , temp2)
    
genLazyTacRel'' (Temp mode1 id1 loc ty) (Temp _ id2 _ _) op label = do
  let tempTrue = Temp mode1 "true" loc ty
  let tempFalse = Temp mode1 "false" loc ty
  case op of
    ThreeAddressCode.TAC.NEQ -> 
      if id1 /= id2
      then return ([TACEntry Nothing $ UnconJump label], tempTrue) 
      else return ([],tempFalse)
    ThreeAddressCode.TAC.EQ -> 
      if id1 == id2
      then return ([TACEntry Nothing $ UnconJump label], tempTrue) 
      else return ([],tempFalse)
 
tacCheckerUnaryBoolean expType _loc e1 = do
  tacCheckerReverBooleanLabel
  (tac1,temp1) <- tacGeneratorExpression expType e1
  return (tac1, temp1)

tacCheckerReverBooleanLabel = do
  labels@(SequenceLazyEvalLabels ltrue lfalse lbreak) <- popSequenceControlLabels
  setSequenceControlLabels labels
  modify $ addIfSimpleLabels lfalse ltrue lbreak


tacGeneratorFunctionCall expType identifier@(PIdent (_, id)) paramsPassed = do
  tacEnv <- get
  tacParameters <- mapM (tacGeneratorFunctionParameter expType) paramsPassed
  let tacExp = concatMap fst tacParameters
  let tempPars =  map snd tacParameters
  let (loc,funParams) = getFunctionParams identifier tempPars tacEnv 
  let Function _ retTy = getVarTypeTAC identifier tacEnv
  let parameterLenght = length paramsPassed
  tacPar <- mapM tacGenerationTempParameter $ zip funParams tempPars
  let funTemp = Temp ThreeAddressCode.TAC.Variable id loc retTy
  let tacsTemp = reverse $ map (tacGenerationEntryParameter.snd) tacPar
  let tacs = reverse $ tacsTemp ++ concatMap fst tacPar ++ tacExp 
  case retTy of
    Utils.Type.Void -> do
      let tacEntry = TACEntry Nothing $ CallProc funTemp parameterLenght
      return (tacs ++ [ tacEntry],funTemp)
    _ -> do
      idResFun <- newtemp
      let idResTemp = Temp ThreeAddressCode.TAC.Temporary idResFun loc retTy
      let tacEntry = TACEntry Nothing $ CallFun idResTemp funTemp parameterLenght
      return (tacEntry :reverse tacs , idResTemp)


tacGeneratorFunctionParameter expType (PassedPar exp) =
  case exp of
    Evar identifier@(PIdent (_, id)) -> do
      tacEnv <- get
      let Checker.SymbolTable.Variable loc ty = getVarTypeTAC identifier tacEnv
      let varTemp =  Temp ThreeAddressCode.TAC.Variable id loc ty 
      return ([], varTemp)
    _ -> tacGeneratorExpression expType exp

tacGenerationEntryParameter temp = TACEntry Nothing $ SetParam temp

tacGenerationTempParameter (Checker.SymbolTable.Variable locMode tyPar,temp@(Temp _ _ _ tyTemp)) = 
    case (tyPar, tyTemp) of
      (Reference _, Reference _)  -> return ([], temp)
      (_, Reference tyFound) -> do
        idVal <- newtemp 
        let valTemp = Temp ThreeAddressCode.TAC.Temporary idVal locMode tyFound
        let valEntry = TACEntry Nothing $ ReferenceRight valTemp temp
        return ([valEntry],valTemp )
      (Reference tyFound, _) -> do
        idRef <- newtemp
        let refTemp = Temp ThreeAddressCode.TAC.Temporary idRef locMode tyFound
        let refEntry = TACEntry Nothing $ DeferenceRight refTemp temp
        return ([refEntry], refTemp)
      _ -> return ([], temp)


tacGeneratorArrayIndexing var@(Evar identifier@(PIdent (_, id))) arrayDecl = do
  tacEnv <- get
  let Checker.SymbolTable.Variable loc ty = getVarTypeTAC identifier tacEnv
  (tacsAdd, temp) <- tacGeneratorArrayIndexing' var arrayDecl
  let tySubArray = calculateSubArrayTy ty arrayDecl
  idArVal <- newtemp
  let tempArrayVal = Temp ThreeAddressCode.TAC.Temporary idArVal loc tySubArray 
  let tempId = Temp ThreeAddressCode.TAC.Variable id loc ty
  let tacEntry = TACEntry Nothing $ IndexRight tempArrayVal tempId temp
  return (tacEntry:tacsAdd, tempArrayVal)
    where
      calculateSubArrayTy ty (ArrayInit _ expressions _ ) = 
        getSubarrayDimension ty (length expressions)


tacGeneratorArrayIndexing' (Evar identifier) arrayDecl = do
  tacEnv <- get
  let var = getVarTypeTAC identifier tacEnv
  (tacsPos, temps) <- tacGenerationArrayPosition arrayDecl
  (tacsAdd, temp) <- tacGenerationArrayPosAdd var temps
  return (tacsAdd ++ tacsPos, temp)

tacGeneratorAssignment leftExp assign rightExp = do
  (tacs, operation, (Temp _ _ locLeft tyLeft), labelExit) <- tacGeneratorLeftExpression leftExp assign rightExp
  idTemp <- newtemp
  let temp = Temp ThreeAddressCode.TAC.Temporary idTemp locLeft tyLeft
  case operation of 
    Nullary temp1 temp2 -> case assign of
      AssgnEq (PAssignmEq _) -> return (TACEntry labelExit operation:tacs, temp)
      AssgnPlEq (PAssignmPlus _) -> do
        let entryAdd = TACEntry labelExit $ Binary temp1 temp1 Plus temp2
        return (entryAdd:tacs, temp)
    IndexLeft variable@(Temp _ _ _ ty) index tempRight -> case assign of 
      AssgnEq (PAssignmEq _) -> return (TACEntry labelExit operation:tacs, temp)
      AssgnPlEq (PAssignmPlus (loc,_)) -> do
        idTempAdd <- newtemp
        let tempAdd = Temp ThreeAddressCode.TAC.Temporary idTempAdd loc ty
        let entryArrayValue = TACEntry labelExit $ IndexRight temp variable index
        let entryadd = TACEntry Nothing $ Binary tempAdd temp Plus tempRight 
        let entryFinal = TACEntry Nothing $ IndexLeft variable index tempAdd
        return (entryFinal:entryadd:entryArrayValue:tacs, temp)
    ReferenceLeft tempLeft@(Temp _ _ _ ty) tempRight -> case assign of
      AssgnEq (PAssignmEq _) -> return (TACEntry labelExit operation:tacs, temp)
      AssgnPlEq (PAssignmPlus (loc,_)) -> do
        idRefValue <- newtemp
        let tempRefValue = Temp ThreeAddressCode.TAC.Temporary idRefValue loc ty
        let entryPointerValue = TACEntry labelExit $ ReferenceRight tempRefValue tempLeft
        idTempAdd <- newtemp
        let tempAdd = Temp ThreeAddressCode.TAC.Temporary idTempAdd loc ty
        let entryadd = TACEntry Nothing $ Binary tempAdd tempRefValue Plus tempRight 
        let entryPointerAdd = TACEntry Nothing $ ReferenceLeft tempLeft tempAdd
        return (entryPointerAdd:entryadd:entryPointerValue:tacs, temp)

tacGeneratorLeftExpression leftExp assgn rightExp = do
  (tacLeft, opWithoutTempRight, tempLeft@(Temp _ _ _ ty)) <- tacGeneratorLeftExpression' leftExp
  (tacRight, tempRight, labelExit) <- tacGeneratorRightExpression' rightExp (getBasicType ty)
  return (tacRight ++ tacLeft, opWithoutTempRight tempRight, tempLeft, labelExit)
    where
      tacGeneratorLeftExpression' leftExp = case leftExp of
        Evar {} -> do
          (_, varTemp@(Temp _ _ _ ty)) <- tacGeneratorExpression LeftExp leftExp
          case ty of
            Reference _ty -> return ([], ReferenceLeft varTemp, varTemp)
            _ ->  return ([], Nullary varTemp, varTemp)
        Earray expIdentifier arDeclaration -> do
          (_, tempLeft) <- tacGeneratorExpression LeftExp expIdentifier
          (tacsAdd, temp) <- tacGeneratorArrayIndexing' expIdentifier arDeclaration
          return (tacsAdd, IndexLeft tempLeft temp, tempLeft)
        Epreop (Indirection _) e1 -> do
          (tacs, tempLeft) <- tacGeneratorExpression LeftExp e1
          return (tacs, ReferenceLeft tempLeft, tempLeft)
      tacGeneratorRightExpression' rightExp tyLeft = case tyLeft of
        Bool -> tacGeneratorBooleanStatement (getAssignOpPos assgn) rightExp
        _ -> do 
          (tacs,temp) <- tacGeneratorExpression RightExp rightExp 
          return (tacs, temp, Nothing)

tacGenerationArrayPosition (ArrayInit _ expressions _ ) = tacGenerationArrayPosition' expressions
  where
    tacGenerationArrayPosition' [ExprDec x] = do
      (tacExp, tempExp) <- tacGeneratorExpression RightExp x 
      return (tacExp, [tempExp])
    tacGenerationArrayPosition' (ExprDec x:xs) = do
      (tacExp, tempExp) <- tacGeneratorExpression RightExp x 
      (tacOther, tempOthers) <- tacGenerationArrayPosition' xs
      return (tacExp ++ tacOther, tempExp:tempOthers)

tacGenerationArrayPosAdd (Checker.SymbolTable.Variable loc ty) temps = do
  (tacs, temp) <- tacGenerationArrayPosAdd' (tail (reverse (getArrayDimensions ty))) (reverse temps)
  idSize <- newtemp
  let size = sizeof ty
  let tempSize = Temp ThreeAddressCode.TAC.Fixed (show size) loc Int 
  let tempResult =  Temp ThreeAddressCode.TAC.Temporary idSize loc Int 
  let sizeEntry =  TACEntry Nothing $ Binary tempResult temp Times tempSize
  return (sizeEntry:tacs,tempResult)
    where
      tacGenerationArrayPosAdd' _ [x] = return ([], x)
      tacGenerationArrayPosAdd' (x:xs) (y@(Temp _ _ loc _):ys) = do
        (tacs, temp) <- tacGenerationArrayPosAdd' xs ys
        idMultTemp <- newtemp
        idAddTemp <- newtemp
        let tempLength = Temp ThreeAddressCode.TAC.Fixed (show x) loc Int
        let tempMult = Temp ThreeAddressCode.TAC.Temporary idMultTemp loc Int
        let tempAdd = Temp ThreeAddressCode.TAC.Temporary idAddTemp loc Int
        let mulEntry = TACEntry Nothing $ Binary tempMult temp Times tempLength
        let addEntry =  TACEntry Nothing $ Binary tempAdd y Plus tempMult
        return (addEntry:mulEntry:tacs,tempAdd)
