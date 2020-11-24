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
          return $ reverse tacEntry ++ resFirst ++ concat res
        else do
          resFirst <- tacGeneratorArrayIdentifier labelExit temp  (getStartExpPos exp) lengths firstTemp
          res <- mapM (tacGeneratorArrayIdentifier Nothing temp (getStartExpPos exp) lengths ) (tail temps)
          return $ reverse tacEntry ++ resFirst ++ concat res
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

tacGeneratorIdentifier labelExit tempRight tempLeft@(Temp _ _ loc ty) =
  case ty of 
    Array {} -> tacGeneratorArrayIdentifierArray  labelExit tempRight (IndexLeft tempLeft) 0 loc
    _ -> return [TACEntry labelExit (Nullary tempLeft tempRight) noComment]
 

tacGeneratorArrayIdentifier labelExit temp@(Temp _ _ _ tyTemp) _ lenghts (Temp _ idVar locVar _) = do
  tacEnv <- get
  let Checker.SymbolTable.Variable loc tyVar = getVarTypeTACDirect locVar idVar tacEnv
  let arrayPos = arrayCalculatePosition tyVar lenghts
  let temp1 = Temp ThreeAddressCode.TAC.Variable idVar loc (getBasicType tyVar)
  let indexLeft = IndexLeft temp1
  case tyTemp of
    Array {} -> tacGeneratorArrayIdentifierArray labelExit temp indexLeft arrayPos loc
    _ -> tacgeneratorArrayIdentifierNormal labelExit indexLeft temp arrayPos loc

tacgeneratorArrayIdentifierNormal labelExit indexLeft temp arrayPos loc  = do
  let temp2 = Temp ThreeAddressCode.TAC.Fixed (show arrayPos) loc Int 
  let tacArray = TACEntry labelExit (indexLeft temp2 temp) noComment
  return [tacArray]
tacGeneratorArrayIdentifierArray labelExit temp@(Temp _ _ _ ar@Array {}) indexLeft arrayPos loc = do
  let totElements = calculateTotalDim ar
  let sizeElement = sizeof ar
  let actualDim = totElements * arrayPos + totElements * sizeElement - sizeElement 
  generateDimArray indexLeft temp actualDim totElements sizeElement loc
    where
      calculateTotalDim ar@(Array ty _) = getArrayLenght ar * calculateTotalDim ty
      calculateTotalDim _ = 1
      generateDimArray _ _ _ 0 _ _ = return []
      generateDimArray indexLeft temp@(Temp _ _ _ ty) act totalElement sizeElement loc = do
        idTemp <- newtemp
        let tempArray = Temp Temporary idTemp loc (getBasicType ty)
        let tempPosRight = Temp ThreeAddressCode.TAC.Fixed (show ((totalElement - 1 )* sizeElement)) loc Int
        let tempPosLeft = Temp ThreeAddressCode.TAC.Fixed (show act ) loc Int
        let tacPos = TACEntry Nothing (IndexRight tempArray temp tempPosRight) noComment
        let tacValue =  TACEntry labelExit (indexLeft tempPosLeft tempArray) noComment 
        tacs <- generateDimArray indexLeft temp (act - sizeElement) (totalElement - 1) sizeElement loc
        return (tacPos:tacValue:tacs)

arrayCalculatePosition ty lengths = sizeof ty * arrayCalculatePosition' (reverse (getArrayDimensions ty)) lengths
  where
    arrayCalculatePosition' _ [x] =  x 
    arrayCalculatePosition' (y:ys) (x:xs) = x + y * arrayCalculatePosition' ys xs

getArrayDimensions ar@(Array ty _) = getArrayLenght ar:getArrayDimensions ty  
getArrayDimensions _ = []

tacGeneratorFunction (FunDec _ signature body) = do
  envTac <-get
  tacs <- tacGeneratorBody body 
  let ident@(PIdent (loc,id)) = getFunNamePident signature
  let tacVoid = TACEntry Nothing VoidOp (Just ("code of function " ++ id))
  let Function _ retTy = getVarTypeTAC ident envTac
  case retTy of
    Utils.Type.Void -> do
      let entryVoid = TACEntry Nothing ReturnVoid noComment
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
    (_,_,tree,_,_,_) <- get
    let Just node = findFirstParentsBlockTypeFromPos SequenceBlk tree pos
    label <- newlabel (getBpEndPos node) BreakLb
    let breakEntry = TACEntry Nothing (UnconJump label) noComment
    return [breakEntry]
  Continue (PContinue (pos,_)) _semicolon -> do
    (_,_,tree,_,_,_) <- get
    let Just node = findFirstParentsBlockTypeFromPos SequenceBlk tree pos
    label <- newlabel (getBpStartPos node) ContinueLb
    let continueEntry = TACEntry Nothing (UnconJump label) noComment
    return [continueEntry]
  DoWhile (Pdo (loc,_)) _while body (SGuard _ guard _) -> do
    labelJump <- newlabel loc VoidLb
    let entryJump = TACEntry Nothing (UnconJump labelJump) noComment

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
    let gotob = TACEntry Nothing (UnconJump labelBegin) noComment
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
    let gotob = TACEntry Nothing (UnconJump labelBegin) noComment
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
    let goto = TACEntry Nothing (UnconJump latestLabel) noComment

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
    let tacEntry = TACEntry Nothing (ReturnValue temp) noComment
    return $ tacs ++ [tacEntry]
  RetVoid _ret _semicolon -> return [TACEntry Nothing ReturnVoid noComment] 

attachLabelToFirstElem (Just label) [] = [TACEntry (Just label) VoidOp noComment]
attachLabelToFirstElem (Just label) ((TACEntry Nothing _e _comment):xs) = TACEntry (Just label) _e _comment:xs
attachLabelToFirstElem (Just label) ((TACEntry (Just _l) _e _comment):xs) = TACEntry (Just label) VoidOp noComment:TACEntry (Just _l) _e _comment:xs
attachLabelToFirstElem Nothing xs = xs


tacGeneratorExpression expType exp = case exp of
    Epreop (MinusUnary (PEminus (loc,_))) e1 -> 
      case e1 of
        (Epreop (MinusUnary (PEminus (loc,_))) e1') -> tacGeneratorExpression expType e1'
        _ -> tacGeneratorExpressionUnary' expType e1 MinusUnaryOp loc
    EAss e1 assign e2 -> tacGeneratorAssignment e1 assign e2
    Eplus e1 (PEplus (loc,_)) e2 -> tacGeneratorExpression' expType e1 Plus e2 loc
    Emod e1 (PEmod (loc,_)) e2 -> tacGeneratorExpression' expType e1 Modul e2 loc
    Epow e1 (PEpow (loc,_)) e2 -> tacGeneratorExpression' expType e1 Pow e2 loc
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
    Earray expIdentifier arDeclaration -> tacGeneratorArrayIndexing expType expIdentifier arDeclaration
    EFun funidentifier _ passedparams _ -> do
        (tacs, temp) <- tacGeneratorFunctionCall expType funidentifier passedparams
        return (reverse tacs, temp)
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
      let tacTrue = TACEntry labelTrue' (Nullary tempRight tempTrue) noComment
      let tacGoToExit = TACEntry Nothing (UnconJump labelExit) noComment
      let tacFalse = TACEntry (Just labelFalse) (Nullary tempRight tempFalse) noComment
      return (tacFalse:tacGoToExit:tacTrue: reverse resg,tempRight, Just labelExit)
    (ThreeAddressCode.TAC.Variable, Bool) -> return ([], temp, Nothing)
    (Fixed, Bool) -> return ([], temp, Nothing)
    _ -> return (resg, temp, Nothing)
           


tacGeneratorVariable expType identifier@(PIdent ((l,c), id)) = do
  tacEnv <- get
  let Checker.SymbolTable.Variable loc ty = getVarTypeTAC identifier tacEnv
  let varTemp =  Temp ThreeAddressCode.TAC.Variable id loc ty
  case ty of 
    Reference tyRef -> case expType of
      LeftExp ->  return ([], varTemp) 
      _ ->  case tyRef of
        Bool -> do
          (tacs, tempRef) <- createReferenceEntry loc tyRef varTemp
          (tacsBool, temp) <- tacGeneratorBooleanVariable  tempRef
          return (tacs ++ tacsBool, temp)
        _-> createReferenceEntry loc tyRef varTemp
    Bool -> case expType of 
      LeftExp ->  return ([], varTemp) 
      _ -> tacGeneratorBooleanVariable varTemp
    _ -> return ([], varTemp)
    where 
      createReferenceEntry loc tyRef varTemp = do
        idVal <- newtemp
        let valTemp =  Temp ThreeAddressCode.TAC.Temporary idVal loc tyRef
        let entryRef = TACEntry Nothing (ReferenceRight valTemp varTemp) (Just ("var declared at line " ++ show l ++ " column " ++ show c))
        return ([entryRef], valTemp)


tacGeneratorAddress expType loc e = case e of
    InnerExp _ e _ -> tacGeneratorExpression expType e
    Epreop (Indirection _)e1 -> do
      (tacs, Temp mode id  loc ty) <- tacGeneratorExpression expType e1 
      return (tacs,Temp mode id loc (Pointer ty))
    _ -> do
      (tacs, temp@(Temp mod id l ty)) <- tacGeneratorExpression LeftExp e
      case ty of 
        Reference ty -> return (tacs, Temp mod id l ty)
        _ -> do
          defId <- newtemp
          let defTemp = Temp ThreeAddressCode.TAC.Temporary defId loc (Pointer ty)
          let defTac = TACEntry Nothing (DeferenceRight defTemp temp) noComment
          return (defTac:tacs,defTemp)

tacGeneratorPointer expType loc e = case e of
  InnerExp _ e _ -> tacGeneratorPointer expType  loc e
  Epreop (Address _ ) e1 -> do
    (tacs, Temp mode id loc ty) <- tacGeneratorExpression expType e1
    let Pointer tyfound = ty
    return (tacs, Temp mode id loc tyfound)
  _ -> do
    (tacs, temp@(Temp mod id l ty)) <- tacGeneratorExpression expType e
    case ty of 
      Reference ty -> return (tacs, Temp mod id l ty)
      Pointer ty -> do
        refId <- newtemp
        let refTemp = Temp ThreeAddressCode.TAC.Temporary refId loc ty
        let refTac = TACEntry Nothing (ReferenceRight refTemp temp) noComment
        return (refTac:tacs,refTemp)

tacGeneratorExpression' exptype e1 op e2 loc = do
  (tac1,temp1@(Temp _ _ _ ty1)) <- tacGeneratorExpression exptype e1
  (tac2,temp2@(Temp _ _ _ ty2)) <- tacGeneratorExpression exptype e2
  idTemp <- newtemp
  let temp = Temp ThreeAddressCode.TAC.Temporary idTemp loc (supTac Sup ty1 ty2)
  let newEntry = TACEntry Nothing (Binary temp temp1 op temp2) noComment
  let tac = newEntry:(tac1 ++ tac2)
  return (tac, temp)

tacGeneratorExpressionUnary' exptype e1 op loc = do
  (tac1,temp1@(Temp _ _ _ ty1)) <- tacGeneratorExpression exptype e1
  idTemp <- newtemp
  let temp = Temp ThreeAddressCode.TAC.Temporary idTemp loc (supTac Sup Int ty1)
  let newEntry = TACEntry Nothing (Unary temp op temp1) noComment
  let tac = newEntry:tac1
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
        (tacs1, temp1, label1) <- tacGeneratorBooleanStatement (getStartExpPos e1) e1
        (tacs2, temp2, label2) <- tacGeneratorBooleanStatement (getStartExpPos e2) e2
        let tacs = reverse tacs1 ++ attachLabelToFirstElem label1 (reverse tacs2)
        (tacRel, _temp) <- genLazyTacREL label2 temp1 op temp2 ltrue lfalse
        return (tacs ++ tacRel, fakeTemp)
    
genLazyTacAND vtype loc e1 _op e2 ltrue lfalse = 
  if isLabelFALL lfalse
    then do
        l1true <- newlabelFALL loc VoidLb -- TrueBoolStmLb
        l1false <- newlabel loc VoidLb --FalseBoolStmLb

        modify $ addIfSimpleLabels l1true l1false l1false
        (tac1,_) <- tacGeneratorExpression vtype e1

        let l2true = ltrue
        let l2false = lfalse
        modify $ addIfSimpleLabels l2true l2false l1false

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

        let l2true = ltrue
        let l2false = lfalse
        modify $ addIfSimpleLabels l2true l2false l1false
        
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

        let l2true = ltrue
        let l2false = lfalse
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
    else
      do
        l1false <- newlabelFALL loc VoidLb --FalseBoolStmLb
        l1true <- newlabel loc TrueBoolStmLb
        modify $ addIfSimpleLabels l1true l1false l1false
        (tac1,_) <- tacGeneratorExpression vtype e1

        let l2true = ltrue
        let l2false = lfalse
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
    (_, True) -> return ([TACEntry Nothing (UnconJump ltrue) noComment], constTemp)
    (True, _) -> return ([], constTemp)
    (_,_) -> return ([TACEntry Nothing (UnconJump ltrue) noComment], constTemp)

tacGeneratorBooleanVariable varTemp = do
  labels@(SequenceLazyEvalLabels ltrue lfalse _) <- popSequenceControlLabels
  setSequenceControlLabels labels
  case (isLabelFALL ltrue, isLabelFALL lfalse) of
    (True,True) -> return ([], varTemp) 
    (_, True) -> return ([TACEntry Nothing (BoolTrueCondJump varTemp ltrue) noComment], varTemp)
    (True, _) -> return ([TACEntry Nothing (BoolFalseCondJump varTemp lfalse) noComment], varTemp)
    (_,_) -> return ([TACEntry Nothing (BoolTrueCondJump varTemp ltrue) noComment,TACEntry Nothing (UnconJump lfalse) noComment], varTemp)

genLazyTacREL label temp1@(Temp mode1 _ _ ty) op temp2 ltrue lfalse =
  case (isLabelFALL ltrue, isLabelFALL lfalse) of
    (True,True) -> return ([], temp1)
    (_, True) -> do
          let aa = TACEntry label (RelCondJump temp1 op temp2 ltrue) noComment
          return ([aa], temp2)
    (True, _) -> do
          let ope = notRel op
          let aa = TACEntry label (RelCondJump temp1 ope temp2 lfalse) noComment
          return ([aa], temp2)
    (_,_) -> do 
          let aa = TACEntry label (RelCondJump temp1 op temp2 ltrue) noComment
          let bb = TACEntry Nothing (UnconJump lfalse) noComment
          return ([aa,bb] , temp2)
    
genLazyTacRel'' (Temp mode1 id1 loc ty) (Temp _ id2 _ _) op label = do
  let tempTrue = Temp mode1 "true" loc ty
  let tempFalse = Temp mode1 "false" loc ty
  case op of
    ThreeAddressCode.TAC.NEQ -> 
      if id1 /= id2
      then return ([TACEntry Nothing (UnconJump label) noComment], tempTrue) 
      else return ([],tempFalse)
    ThreeAddressCode.TAC.EQ -> 
      if id1 == id2
      then return ([TACEntry Nothing (UnconJump label) noComment], tempTrue) 
      else return ([],tempFalse)
 
tacCheckerUnaryBoolean expType loc e1 = case e1 of
  Evar (PIdent (loc,id)) -> do
    tacCheckerReverBooleanLabel
    let tempVar = Temp Temporary id loc Bool
    (tacs, Temp _ loc id ty) <- tacGeneratorBooleanVariable tempVar
    return (tacs, Temp Temporary loc id ty)
  Econst constant -> case constant of
    ETrue (PTrue (loc,_)) -> return ([], Temp ThreeAddressCode.TAC.Fixed "false" loc Bool)
    EFalse (PFalse (loc,_)) -> return ([], Temp ThreeAddressCode.TAC.Fixed "true" loc Bool)
  _ -> do
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
      let tacEntry = TACEntry Nothing (CallProc funTemp parameterLenght) noComment
      return (tacs ++ [tacEntry],funTemp)
    _ -> do
      idResFun <- newtemp
      let idResTemp = Temp ThreeAddressCode.TAC.Temporary idResFun loc retTy
      let tacEntry = TACEntry Nothing (CallFun idResTemp funTemp parameterLenght) noComment
      return (tacs ++ [tacEntry], idResTemp)


tacGeneratorFunctionParameter expType paramPassed = case paramPassed of
  PassedParWMode mode exp ->  tacGeneratorFunctionParameterAux (convertMode mode) exp
  PassedPar exp -> tacGeneratorFunctionParameterAux  Utils.Type.Normal exp
  where 
    tacGeneratorFunctionParameterAux mode exp = 
      case exp of
        Evar identifier@(PIdent (_, id)) -> do
          tacEnv <- get
          let Checker.SymbolTable.Variable loc ty= getVarTypeTAC identifier tacEnv
          let varTemp =  Temp ThreeAddressCode.TAC.Variable id loc (convertTyMode mode ty) 
          return ([], varTemp)
        _ -> do
          (tacs, Temp m id loc ty) <- tacGeneratorExpression expType exp
          return (tacs, Temp m id loc (convertTyMode mode ty))

tacGenerationEntryParameter temp = TACEntry Nothing (SetParam temp) noComment

tacGenerationTempParameter (Checker.SymbolTable.Variable locMode tyPar,temp@(Temp _ _ _ tyTemp)) = 
    case (tyPar, tyTemp) of
      (Reference _, Reference _)  -> return ([], temp)
      (_, Reference tyFound) -> do
        idVal <- newtemp 
        let valTemp = Temp ThreeAddressCode.TAC.Temporary idVal locMode tyFound
        let valEntry = TACEntry Nothing (ReferenceRight valTemp temp) noComment
        return ([valEntry],valTemp )
      (Reference tyFound, _) -> do
        idRef <- newtemp
        let refTemp = Temp ThreeAddressCode.TAC.Temporary idRef locMode tyFound
        let refEntry = TACEntry Nothing (DeferenceRight refTemp temp) noComment
        return ([refEntry], refTemp)
      _ -> return ([], temp)


tacGeneratorArrayIndexing expType exp arrayDecl = case exp of
  InnerExp _ e _ -> tacGeneratorArrayIndexing expType e arrayDecl
  Epreop (Indirection (PEtimes (loc,_))) e1 -> do
    (tacsPoint, tempPoint) <- tacGeneratorPointer expType loc e1 
    (tacsGen, tempGen) <- tacGeneratorArrayIndexingAux tempPoint
    return (tacsGen ++ tacsPoint,tempGen)
  (Evar identifier@(PIdent (_, id))) -> do
    tacEnv <- get
    let Checker.SymbolTable.Variable loc ty = getVarTypeTAC identifier tacEnv
    let tempId = Temp ThreeAddressCode.TAC.Variable id loc ty
    tacGeneratorArrayIndexingAux tempId
  where
    tacGeneratorArrayIndexingAux tempVar@(Temp _ _ loc ty) = do
      (tacsAdd, temp) <- tacGeneratorArrayIndexing' (loc,ty) arrayDecl
      let tySubArray = calculateSubArrayTy ty arrayDecl
      idArVal <- newtemp
      let tempArrayVal = Temp ThreeAddressCode.TAC.Temporary idArVal loc tySubArray 
      let tacEntry = TACEntry Nothing (IndexRight tempArrayVal tempVar temp) noComment
      return (tacEntry:tacsAdd, tempArrayVal)
    calculateSubArrayTy ty (ArrayInit _ expressions _ ) = 
      getSubarrayDimension ty (length expressions)


tacGeneratorArrayIndexing' (loc,ty) arrayDecl = do
  (tacsPos, temps) <- tacGenerationArrayPosition arrayDecl
  (tacsAdd, temp) <- tacGenerationArrayPosAdd (loc,ty) temps
  return (tacsAdd ++ tacsPos, temp)

tacGeneratorAssignment leftExp assign rightExp = do
  (tacs, operation, (Temp _ _ locLeft tyLeft), labelExit) <- tacGeneratorLeftExpression leftExp assign rightExp
  idTemp <- newtemp
  let temp = Temp ThreeAddressCode.TAC.Temporary idTemp locLeft tyLeft
  case operation of 
    Nullary temp1 temp2 -> case assign of
      AssgnEq (PAssignmEq _) -> return (TACEntry labelExit operation noComment:tacs, temp)
      AssgnPlEq (PAssignmPlus _) -> do
        let entryAdd = TACEntry labelExit (Binary temp1 temp1 Plus temp2) noComment
        return (entryAdd:tacs, temp)
    IndexLeft variable@(Temp _ _ _ ty) index tempRight -> case assign of 
      AssgnEq (PAssignmEq _) -> return (TACEntry labelExit operation noComment:tacs, temp)
      AssgnPlEq (PAssignmPlus (loc,_)) -> do
        idTempAdd <- newtemp
        let tempAdd = Temp ThreeAddressCode.TAC.Temporary idTempAdd loc ty
        let entryArrayValue = TACEntry labelExit (IndexRight temp variable index) noComment
        let entryadd = TACEntry Nothing (Binary tempAdd temp Plus tempRight) noComment
        let entryFinal = TACEntry Nothing (IndexLeft variable index tempAdd) noComment
        return (entryFinal:entryadd:entryArrayValue:tacs, temp)
    ReferenceLeft tempLeft@(Temp _ _ _ ty) tempRight -> case assign of
      AssgnEq (PAssignmEq _) -> return (TACEntry labelExit operation noComment:tacs, temp)
      AssgnPlEq (PAssignmPlus (loc,_)) -> do
        idRefValue <- newtemp
        let tempRefValue = Temp ThreeAddressCode.TAC.Temporary idRefValue loc ty
        let entryPointerValue = TACEntry labelExit (ReferenceRight tempRefValue tempLeft) noComment
        idTempAdd <- newtemp
        let tempAdd = Temp ThreeAddressCode.TAC.Temporary idTempAdd loc ty
        let entryadd = TACEntry Nothing (Binary tempAdd tempRefValue Plus tempRight) noComment
        let entryPointerAdd = TACEntry Nothing (ReferenceLeft tempLeft tempAdd) noComment
        return (entryPointerAdd:entryadd:entryPointerValue:tacs, temp)

tacGeneratorLeftExpression leftExp assgn rightExp = do
  (tacLeft, opWithoutTempRight, tempLeft@(Temp _ _ _ ty)) <- tacGeneratorLeftExpression' leftExp
  (tacRight, tempRight, labelExit) <- tacGeneratorRightExpression' rightExp (getBasicType ty)
  return (tacRight ++ tacLeft, opWithoutTempRight tempRight, tempLeft, labelExit)
    where
      tacGeneratorLeftExpression' leftExp = case leftExp of
        InnerExp _ e _ -> tacGeneratorLeftExpression'  e
        Evar {} -> do
          (_, varTemp@(Temp _ _ _ ty)) <- tacGeneratorExpression LeftExp leftExp
          case ty of
            Reference _ty -> return ([], ReferenceLeft varTemp, varTemp)
            _ ->  return ([], Nullary varTemp, varTemp)
        Earray expIdentifier arDeclaration -> do
          (tacLeft, tempLeft@(Temp _ _ loc ty)) <- tacGeneratorExpression LeftExp expIdentifier
          (tacsAdd, temp) <- tacGeneratorArrayIndexing' (loc,ty) arDeclaration
          return (tacsAdd ++ tacLeft, IndexLeft tempLeft temp, tempLeft)
        Epreop (Indirection _) e1 -> do
          (tacs, tempLeft) <- tacGeneratorExpression LeftExp leftExp
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

tacGenerationArrayPosAdd (loc,ty) temps = do
  (tacs, temp) <- tacGenerationArrayPosAdd' (tail (reverse (getArrayDimensions ty))) (reverse temps)
  idSize <- newtemp
  let size = sizeof ty
  let tempSize = Temp ThreeAddressCode.TAC.Fixed (show size) loc Int 
  let tempResult =  Temp ThreeAddressCode.TAC.Temporary idSize loc Int 
  let sizeEntry =  TACEntry Nothing (Binary tempResult temp Times tempSize) noComment
  return (sizeEntry:tacs,tempResult)
    where
      tacGenerationArrayPosAdd' pos [y] = do
        let offset = getArrayOffset (getSubarrayDimension ty (length pos))
        if offset /= 0
          then do
            idOffsetRemove <- newtemp
            let tempOffset = Temp ThreeAddressCode.TAC.Fixed (show offset) loc Int
            let tempOffsetRemove = Temp ThreeAddressCode.TAC.Temporary idOffsetRemove loc Int
            let offsetEntry = TACEntry Nothing (Binary tempOffsetRemove y Minus tempOffset) noComment
            return ([offsetEntry], tempOffsetRemove)
          else return ([], y)
      tacGenerationArrayPosAdd' pos@(x:xs) (y@(Temp _ _ loc _):ys) = do
        let offset = getArrayOffset (getSubarrayDimension ty (length pos))
        (tacs, temp) <- tacGenerationArrayPosAdd' xs ys
        idMultTemp <- newtemp
        idAddTemp <- newtemp
        let tempLength = Temp ThreeAddressCode.TAC.Fixed (show x) loc Int
        let tempMult = Temp ThreeAddressCode.TAC.Temporary idMultTemp loc Int
        let tempAdd = Temp ThreeAddressCode.TAC.Temporary idAddTemp loc Int
        let mulEntry = TACEntry Nothing (Binary tempMult temp Times tempLength) noComment
        let addEntry =  TACEntry Nothing (Binary tempAdd y Plus tempMult) noComment
        if offset /= 0
          then do
            idOffsetRemove <- newtemp
            let tempOffset = Temp ThreeAddressCode.TAC.Fixed (show offset) loc Int
            let tempOffsetRemove = Temp ThreeAddressCode.TAC.Temporary idOffsetRemove loc Int
            let offsetEntry = TACEntry Nothing (Binary tempOffsetRemove y Minus tempOffset) noComment
            let addEntry =  TACEntry Nothing (Binary tempAdd tempOffsetRemove Plus tempMult) noComment
            return (addEntry:offsetEntry:mulEntry:tacs,tempAdd)
          else return (addEntry:mulEntry:tacs,tempAdd)


-- Functions used to enrich TAC adding necessary casts

startCastTacState = (0, 0, [])

type TacCastState = State (Int,Int,[TACEntry])

newcasttemp :: TacCastState String
newcasttemp = do
  (k, _w, _t) <- get
  put (k + 1, _w, _t)
  return $ int2AddrCastTempName k

int2AddrCastTempName k = "cast" ++ show k

newStringtemp :: TacCastState String
newStringtemp = do
  (_k, w, _t) <- get
  put (_k, w + 1, _t)
  return $ int2AddrStringTempName w
  
int2AddrStringTempName w = "ptr$str$" ++ show w

addTacStringEntry id const loc = do
  let tacString = TACEntry (Just (id,loc,StringLb)) (StringOp const) noComment
  (_k, _w, tacs) <- get
  put (_k, _w, tacString:tacs)
  get
  
-- tacCastGenerator :: Traversable t => t TACEntry -> t [TACEntry]
tacCastGenerator tac = evalState (tacCastGeneratorModified tac) startCastTacState

tacCastGeneratorModified tac = do
  tacs' <- mapM tacCastGenerator' tac
  let tacs = concat tacs'
  (_, _, tacsString) <- get
  if null tacsString
  then return tacs
  else do
    let tacSpace = TACEntry Nothing VoidOp noComment
    let tacComment = TACEntry Nothing (CommentOp "static data") noComment
    return $ tacs ++ tacSpace:tacComment:tacSpace:reverse tacsString


tacCastGenerator' ent = do
  ent' <- tacCastStringGenerator ent
  tacCastGeneratorAux ent'

tacCastStringGenerator (TACEntry label optype _comment) = do
  operation <- tacCastStringGenerator' optype
  return $ TACEntry label operation _comment
  where
    tacCastStringGenerator' optype = case optype of
      Nullary temp1 temp2 -> do
        temp <- tacCastStringGenerator'' temp2
        return $ Nullary temp1 temp
      IndexLeft temp1 temp2 temp3 -> do
        temp <- tacCastStringGenerator'' temp3
        return $ IndexLeft temp1 temp2 temp
      SetParam temp1 -> do
        temp <- tacCastStringGenerator'' temp1
        return $ SetParam temp
      ReturnValue temp1 -> do
        temp <- tacCastStringGenerator'' temp1
        return $ ReturnValue temp
      _ -> return optype
    tacCastStringGenerator'' temp@(Temp mode id loc ty) = case (mode,ty) of
      (Fixed,Utils.Type.String) -> do
        idString <- newStringtemp
        addTacStringEntry idString id loc
        return $ Temp Temporary idString loc ty
      _ -> return temp

checkIfLabelIsAStartOfFun Nothing = False
checkIfLabelIsAStartOfFun (Just (_,_,FunLb)) = True
checkIfLabelIsAStartOfFun (Just _) = False

--tacCastGeneratorAux :: TACEntry -> [TACEntry]
tacCastGeneratorAux ent@(TACEntry label optype _comment) = -- tac
  case optype of
    Binary temp0 temp1 op temp2 -> 
      let ty0 = getTacTempTye temp0
          ty1 = getTacTempTye temp1
          ty2 = getTacTempTye temp2 in
      if ty0 == ty1 && ty1 == ty2
        then 
          return [ent]
        else
          let suptype01 = supTac Sup ty0 ty1
              suptype02 = supTac Sup ty0 ty2
              suptype = supTac Sup suptype01 suptype02 in do
                (tac1,newtemp1) <- genCast temp1 suptype 
                (tac2,newtemp2) <- genCast temp2 suptype
                if checkIfLabelIsAStartOfFun label
                  then return $ attachLabelToFirstElem label tac1 ++ tac2 ++ resetLabelToNothing (substituteVarNames ent newtemp1 newtemp2 suptype)
                  else return $ tac1 ++ tac2 ++ substituteVarNames ent newtemp1 newtemp2 suptype
    Nullary temp1 temp2 -> 
      let ty1 = getTacTempTye temp1
          ty2 = getTacTempTye temp2 in
      if ty1 == ty2
        then return [ent]
        else 
          let suptype = supTac Sup ty1 ty2 in do
            (tac1,newtemp1) <- genCast temp1 suptype 
            (tac2,newtemp2) <- genCast temp2 suptype
            if checkIfLabelIsAStartOfFun label
              then return $ attachLabelToFirstElem label tac1 ++ tac2 ++ resetLabelToNothing (substituteVarNames ent newtemp1 newtemp2 suptype)
              else return $ tac1 ++ tac2 ++ substituteVarNames ent newtemp1 newtemp2 suptype
    Unary temp1 op temp2 -> 
      case op of
        MinusUnaryOp -> 
          let ty1 = getTacTempTye temp1
              ty2 = getTacTempTye temp2 in
          if ty1 == ty2
            then return [ent]
            else 
              let suptype = supTac Sup ty1 ty2 in do
                (tac2,newtemp2) <- genCast temp2 suptype
                if checkIfLabelIsAStartOfFun label
                  then return $ attachLabelToFirstElem label tac2 ++ resetLabelToNothing (substituteVarNames ent temp1 newtemp2 suptype)
                  else return $ tac2 ++ substituteVarNames ent temp1 newtemp2 suptype

                -- return $ tac2 ++ substituteVarNames ent temp1 newtemp2 suptype

    RelCondJump temp1 relop temp2 _label -> 
      let ty1 = getTacTempTye temp1
          ty2 = getTacTempTye temp2 in
      if ty1 == ty2
        then return [ent]
        else 
          let suptype = supTac Sup ty1 ty2 in do
            (tac1,newtemp1) <- genCast temp1 suptype 
            (tac2,newtemp2) <- genCast temp2 suptype
            if checkIfLabelIsAStartOfFun label
              then return $ attachLabelToFirstElem label tac1 ++ tac2 ++ resetLabelToNothing (substituteVarNames ent newtemp1 newtemp2 suptype)
              else return $ tac1 ++ tac2 ++ substituteVarNames ent newtemp1 newtemp2 suptype
    IndexLeft (Temp _ _ _ (Array ty1 _)) _ temp3 -> 
      let ty3 = getTacTempTye temp3 in
        if ty1 == ty3
          then return [ent]
          else
            let suptype = supTac Sup ty1 ty3 in do
              (tac3,newtemp3) <- genCast temp3 suptype
              if checkIfLabelIsAStartOfFun label
                then return $ attachLabelToFirstElem label tac3 ++ resetLabelToNothing (substituteVarNames ent newtemp3 newtemp3 suptype)
                else return $ tac3 ++ substituteVarNames ent newtemp3 newtemp3 suptype

              -- return $ tac3 ++ substituteVarNames ent newtemp3 newtemp3 suptype
    _ -> return [ent]

genCast t@(Temp _ _ loc origtye) destCastType = 
  do
    newt <- newcasttemp
    let newtemp1 = Temp ThreeAddressCode.TAC.Temporary newt loc destCastType
    case (origtye,destCastType) of
      (Int, Real) ->
        return ([TACEntry Nothing (Cast newtemp1 CastIntToFloat t) noComment], newtemp1)
      (Int, Int) ->
        return ([], t)
      (Real, Real) ->
        return ([], t)
      (Char, Char) ->
        return ([], t)
      (Char, Int) ->
        return ([TACEntry Nothing (Cast newtemp1 CastCharToInt t) noComment], newtemp1)
      (Char, Real) ->
        return ([TACEntry Nothing (Cast newtemp1 CastCharToFloat t) noComment], newtemp1)
      (Int, Char) ->
        return ([TACEntry Nothing (Cast newtemp1 CastIntToChar t) noComment], newtemp1)
      (String, String) ->
        return ([], t)
      (Pointer p, Int) ->
        return ([], t)
      (Pointer p, Real) ->
        return ([], t)
      (Array t1 b1, Array t2 b2) ->
        return ([], t)
      (Array t1 b1, Int) ->
        return ([], t)
      (_,_) -> 
        return ([], t)



substituteVarNames ent@(TACEntry label optype _comment) newtemp1 newtemp2 newtye = 
  case optype of
    Binary temp0 temp1 op temp2 -> [TACEntry label (Binary (changeTypeOfTemp temp0 newtye) newtemp1 op newtemp2) _comment]
    RelCondJump temp1 relop temp2 labelg -> [TACEntry label (RelCondJump newtemp1 relop newtemp2 labelg) _comment]
    Nullary temp1 temp2 -> [TACEntry label (Nullary newtemp1 newtemp2) _comment]
    IndexLeft temp1 temp2 temp3 -> [TACEntry label (IndexLeft (changeTypeOfTemp temp1 newtye) temp2 newtemp2) _comment]
    Unary temp1 op temp2 -> [TACEntry label (Unary (changeTypeOfTemp temp1 newtye) op newtemp2) _comment]

resetLabelToNothing [ent@(TACEntry label optype _comment)] = [TACEntry Nothing optype _comment]

changeTypeOfTemp t@(Temp a b c _origtye) destCastType = Temp a b c destCastType