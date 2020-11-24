module Checker.TypeChecker where

import Control.Monad.Trans.State
import qualified Data.Map as DMap
import AbsChapel
import Checker.SymbolTable
import Checker.BPTree
import Checker.SupTable
import Utils.AbsUtils
import Utils.Type
import Checker.ErrorPrettyPrinter


startState = ([], Checker.BPTree.Node {Checker.BPTree.id = ("0", ((0::Int,0::Int),(0::Int,0::Int))), val = BP {symboltable = DMap.empty, statements = [], errors = [], blocktype = ExternalBlk}, parentID = Nothing, children = []}, ("0", ((0::Int,0::Int),(0::Int,0::Int))))

typeChecker (Progr p) = typeCheckerModule p

typeCheckerModule (Mod m) = do
  modify (addFunctionOnCurrentNode "writeInt" (-1,-1) [Variable (-1,-1) Int] Utils.Type.Void)
  modify (addFunctionOnCurrentNode "writeReal" (-1,-1) [Variable (-1,-1) Real] Utils.Type.Void)
  modify (addFunctionOnCurrentNode "writeChar" (-1,-1) [Variable  (-1,-1) Char] Utils.Type.Void)
  modify (addFunctionOnCurrentNode "writeString" (-1,-1) [Variable  (-1,-1) String] Utils.Type.Void)
  modify (addFunctionOnCurrentNode "readInt" (-1,-1) [] Utils.Type.Int)
  modify (addFunctionOnCurrentNode "readReal" (-1,-1) [] Utils.Type.Real)
  modify (addFunctionOnCurrentNode "readChar" (-1,-1) [] Utils.Type.Char)
  modify (addFunctionOnCurrentNode "readString" (-1,-1) [] Utils.Type.String)
  typeCheckerExt m
  get

createId l c name = show l ++ "_" ++ show c ++ "_" ++ name
createId' _ _ name = name

typeCheckerExt [] = get

typeCheckerExt (x:xs) = case x of
    ExtDecl (Decl _ declList _ ) -> do
        mapM_ typeCheckerDeclaration declList
        typeCheckerExt xs
    ExtFun fun -> do
        typeCheckerFunction fun
        typeCheckerExt xs

typeCheckerDeclaration x = do
  (_,_t,_a) <- get
  case x of
    AssgmTypeDec ids _colon types assignment exp -> do
      let DataChecker tyRight errors = typeCheckerDeclExpression (typeCheckerConvertIdsToVariable ids,_t,_a) exp
      tyLeft <- typeCheckerTypeSpecification types
      typeCheckerIdentifiers ids (Just assignment) tyLeft tyRight (Just exp)
      modify $ addErrorsCurrentNode errors
      get
    NoAssgmArrayDec ids _colon types -> do
      ty <- typeCheckerTypeSpecification types
      modify (addErrorsCurrentNode (map (\(PIdent (loc,id))-> ErrorChecker loc $ ErrorMissingInitialization id) ids))
      typeCheckerIdentifiers ids Nothing ty Infered Nothing
    AssgmDec ids assignment exp -> do
      let DataChecker ty errors = typeCheckerDeclExpression (typeCheckerConvertIdsToVariable ids,_t,_a) exp
      modify $ addErrorsCurrentNode errors
      typeCheckerIdentifiers ids (Just assignment) Infered ty (Just exp)
      get

typeCheckerConvertIdsToVariable = map (\(PIdent (loc,id)) -> (id,(id, Variable loc Infered)))

typeCheckerTypeSpecification tySpec = case tySpec of
  TypeSpecNorm ty -> convertTypeSpecToTypeInferred ty
  TypeSpecAr ar ty -> do
    environment <- get
    tyFound <- convertTypeSpecToTypeInferred ty
    let DataChecker typeArray errors = typeCheckerBuildArrayType environment ar tyFound
    modify (addErrorsCurrentNode errors)
    return typeArray



convertTypeSpecToTypeInferred Tint {} = return Int
convertTypeSpecToTypeInferred Treal {} = return Real
convertTypeSpecToTypeInferred Tchar {} = return Char
convertTypeSpecToTypeInferred Tstring {} =  return String
convertTypeSpecToTypeInferred Tbool {} =  return Bool
convertTypeSpecToTypeInferred (TPointer _ ty) = do
  tyFound <- typeCheckerTypeSpecification ty
  return (Pointer tyFound)


typeCheckerIdentifiers identifiers assgn typeLeft typeRight exp = do
  mapM_ (typeCheckerIdentifier assgn typeLeft typeRight exp) identifiers
  get


typeCheckerIdentifier assgn typeLeft typeRight exp id@(PIdent (loc, identifier))  = do
  (_s,_,current_id) <- get
  let (ty, compatibility) = sup SupDecl typeLeft typeRight 
  let errorSup = createNewError (errorIncompatibleDeclarationType loc identifier typeLeft typeRight exp) compatibility
  modify $ addErrorsCurrentNode (errorSup ++ typeCheckerAssignmentDecl identifier assgn)
  case ty of
    Error -> get
    _ -> do
      modify (\(_s,tree,_i) -> (_s, typeCheckerVariable id tree ty current_id, _i ))
      get

typeCheckerFunction (FunDec (PProc (loc@(l,c),_) ) signature body@(BodyBlock  (POpenGraph (locGraph,_)) _ _)) = do
  tyReturn <- typeCheckerSignature loc locGraph signature
  typeCheckerBody (ProcedureBlk tyReturn) (createId' l c (getFunName signature)) body
  get  

typeCheckerSignature locstart locEnd signature = case signature of
  SignNoRet identifier (FunParams (POpenParenthesis (locParamStart,_)) params (PCloseParenthesis (locParamEnd,_))) -> do
    let tyReturn = Utils.Type.Void
    typeCheckerSignature' locstart locEnd locParamStart locParamEnd identifier params tyReturn
    return tyReturn
  SignWRet identifier (FunParams (POpenParenthesis (locParamStart,_)) params (PCloseParenthesis (locParamEnd,_))) _ types -> do
    tyReturn <- typeCheckerTypeSpecification types
    typeCheckerSignature' locstart locEnd locParamStart locParamEnd identifier params tyReturn
    return tyReturn

typeCheckerSignature' locstart locEnd locParamStart locParamEnd ident@(PIdent (loc,identifier)) params types = 
  case types of
    Error -> get
    _ -> do
      typeCheckerParams params
      (symtable, tree, currentIdNode) <- get
      let node = findNodeById currentIdNode tree
      let DataChecker entry errors = getEntry ident (symtable, tree, currentIdNode)
      let variables = reverse $ map (snd.snd) symtable
      case entry of
        Nothing -> do
          modify $ addErrorsCurrentNode errors
          put (symtable, updateTree (addEntryNode identifier (Function [(loc,variables)] types) node) tree, currentIdNode )
          get
        Just entryFound -> case entryFound of 
          Variable locVar _ -> do
            modify $ addErrorsCurrentNode [ErrorChecker loc $ ErrorVarAlreadyDeclared locVar identifier]
            get
          Function varOfvar t -> do
            let errorsOverloading = typeCheckerSignatureOverloading (locParamStart,locParamEnd) identifier loc variables varOfvar
            let (_,compatibility) = sup SupFun types t
            let convertErrors = createNewError (errorOverloadingReturn loc locstart locEnd identifier types t) compatibility
            if null errors 
            then do
              put (symtable, updateTree (modFunAddOverloading identifier loc variables node) tree, currentIdNode )
              modify $ addErrorsCurrentNode $ errorsOverloading ++ convertErrors
              get
            else do
              modify $ addErrorsCurrentNode (errorsOverloading ++ convertErrors)
              get 

typeCheckerSignatureOverloading _ _ _ _ [] = []
typeCheckerSignatureOverloading  (locstart,locEnd) identifier loc var ((locFun,x):xs) = case (var,x) of
  ([],[]) -> [ErrorChecker loc $ ErrorSignatureAlreadyDeclared (locstart,locEnd) locFun identifier]
  ([],_) -> typeCheckerSignatureOverloading (locstart,locEnd) identifier loc var xs
  _ -> let errors = typeCheckerSignatureOverloading' (locstart,locEnd) identifier loc locFun var x in 
    if null errors
    then typeCheckerSignatureOverloading (locstart,locEnd) identifier loc var xs
    else errors

typeCheckerSignatureOverloading' (locstart,locEnd) identifier loc locFun [] [] = [ErrorChecker loc $ ErrorSignatureAlreadyDeclared (locstart,locEnd) locFun identifier]
typeCheckerSignatureOverloading' _ _ _ _ [] _ = []
typeCheckerSignatureOverloading' _ _ _ _ _ [] = []
typeCheckerSignatureOverloading'  (locstart,locEnd) identifier loc locFun ((Variable _ ty1):xs) ((Variable _ ty2):ys) = 
  if show ty1 == show ty2
  then typeCheckerSignatureOverloading' (locstart,locEnd) identifier loc locFun xs ys
  else []
  
typeCheckerParams xs = do
   mapM_ typeCheckerParam xs
   get

typeCheckerParam x = case x of
  ParNoMode identifiers _ types ->  do
    mapM_ (typeCheckerParam' Normal types) identifiers
    get
  ParWMode mode identifiers  _ types ->  do
    mapM_ (typeCheckerParam' (convertMode mode) types) identifiers
    get

typeCheckerParam' mode tySpec identifier = do 
    ty <- typeCheckerTypeSpecification tySpec
    modify (typeCheckerParam'' identifier mode ty)
    get

typeCheckerParam'' (PIdent (loc,identifier)) mode ty env@(sym,_t,_i) = case typeCheckerParam''' sym identifier of
  Just (_,(_, Variable (varAlreadyDecLine,varAlreadyDecColumn) _ )) -> 
    addErrorsCurrentNode [ErrorChecker (varAlreadyDecLine,varAlreadyDecColumn) $ ErrorVarAlreadyDeclared loc identifier] env
  Nothing -> ((identifier, (identifier, Variable loc (typeCheckerModeParameter mode ty))):sym,_t,_i)

typeCheckerParam''' [] _ = Nothing;
typeCheckerParam''' (x@(id,_):xs) identifier = 
  if id == identifier
    then Just x
    else typeCheckerParam''' xs identifier

typeCheckerModeParameter mode ty = case mode of
  Ref -> Reference ty
  _ -> ty
  
typeCheckerBody blkType identifier (BodyBlock  (POpenGraph (locStart,_)) xs (PCloseGraph (locEnd,_))  ) = do
  (sym, tree, current_id) <- get
  let actualNode = findNodeById current_id tree; child = createChild identifier blkType locStart locEnd actualNode in
     put ([], addChild actualNode (setSymbolTable (DMap.fromList sym) child) tree, (identifier, (locStart, locEnd)))
  mapM_ typeCheckerBody' xs
  modify(\(_s,_t,_) -> (_s,_t,current_id))
  get

typeCheckerBody' x = do
  case x of
    Stm statement -> typeCheckerStatement statement
    Fun fun _ -> typeCheckerFunction fun
    DeclStm (Decl _ declList _ ) -> do
      mapM_ typeCheckerDeclaration declList
      get
    Block body@(BodyBlock (POpenGraph ((l,c), name)) _ _) -> typeCheckerBody SimpleBlk (createId l c name) body
  get

typeCheckerSequenceStatement pos = do
  (_,tree,current_id) <- get
  case findSequenceControlGetBlkType tree current_id of
    WhileBlk -> get
    DoWhileBlk -> get
    _otherwhise -> do
      modify $ addErrorsCurrentNode [ErrorChecker pos ErrorBreakNotInsideAProcedure]
      get

typeCheckerStatement statement = case statement of
  Break (PBreak (pos, _)) _semicolon -> do
    typeCheckerSequenceStatement pos
    get
  Continue (PContinue (pos, _)) _semicolon -> do
    typeCheckerSequenceStatement pos
    get 
  DoWhile (Pdo ((l,c),name)) _while body guard -> do
    typeCheckerBody DoWhileBlk (createId l c name) body
    typeCheckerGuard guard
    get
  While (PWhile ((l,c), name)) guard body -> do
    typeCheckerGuard guard
    typeCheckerBody WhileBlk (createId l c name) body
    get
  If (PIf ((l,c), name)) guard _then body -> do
    typeCheckerGuard guard
    typeCheckerBody IfSimpleBlk (createId l c name) body
    get
  IfElse (PIf ((lIf,cIf), nameIf)) guard _then bodyIf (PElse ((lElse,cElse), nameElse)) bodyElse -> do
    typeCheckerGuard guard
    typeCheckerBody IfThenBlk (createId lIf cIf nameIf) bodyIf
    typeCheckerBody IfElseBlk (createId lElse cElse nameElse) bodyElse
    get
  StExp (EFun funidentifier _ params _) _semicolon-> do
    environment <- get
    let DataChecker _ errors = eFunTypeChecker funidentifier params environment
    modify $ addErrorsCurrentNode errors
    get
  StExp exp _semicolon -> do
    env <- get
    case exp of 
      EAss {} -> do
        let DataChecker _ errors = typeCheckerExpression env exp
        modify $ addErrorsCurrentNode errors
        get
      _ -> do 
        modify $ addErrorsCurrentNode [ ErrorChecker (getStartExpPos exp) $ ErrorOnlyRightExpression exp]
        get
  ret@(RetVal return exp _semicolon) -> do
    (_s,tree,current_id) <- get
    let DataChecker tyret errs2 = typeCheckerExpression (_s,tree,current_id) exp 
    modify $ addErrorsCurrentNode errs2
    typeCheckerReturn return tyret (Just exp)
    modify $ addStatementCurrentNode ret
    get

  ret@(RetVoid return _semicolon) -> do
    typeCheckerReturn return Utils.Type.Void Nothing
    modify $ addStatementCurrentNode ret
    get

typeCheckerReturn (PReturn (pos, _ret)) tyret exp = do 
  (_s,tree,current_id) <- get
  case findProcedureGetBlkType tree current_id of
    ProcedureBlk _ -> do
      let DataChecker tyfun errs1 = getFunRetType (_s,tree,current_id);
      let (_,compatibility) = sup SupRet tyret tyfun 
      let returnError = createNewError (errorIncompatibleReturnType pos (getFunNameFromEnv (_s,tree,current_id)) tyfun tyret exp) compatibility
      let errors = errs1 ++ returnError
      modify $ addErrorsCurrentNode errors
      get
    _otherwhise -> do
      let node = findNodeById current_id tree ; errors = ErrorChecker pos ErrorReturnNotInsideAProcedure
      modify (\(_s, tree,_i) -> (_s, updateTree (addErrorNode errors node) tree , _i ))
      get
  get

typeCheckerGuard (SGuard _ expression _) = do
  (_s,tree,current_id) <- get
  let DataChecker ty errors = typeCheckerExpression (_s,tree,current_id) expression
  modify $ addErrorsCurrentNode errors
  case ty of
    Bool -> get
    _otherwhise -> do
    modify $ addErrorsCurrentNode  [ErrorChecker (getStartExpPos expression) $ ErrorGuardNotBoolean expression ty]
    get


typeCheckerBuildArrayType environment (ArrayDeclIndex _ array _) = typeCheckerBuildArrayType' environment array
  where
    typeCheckerBuildArrayType' _ [] types = DataChecker types []
    typeCheckerBuildArrayType' enviroment (array:arrays) types = 
      let DataChecker bounds errors = typeCheckerBuildArrayParameter array; DataChecker typesFound othersErrors = typeCheckerBuildArrayType' enviroment arrays types in 
        DataChecker (Array typesFound bounds) (errors ++ othersErrors)



typeCheckerAssignmentDecl id assgnm = case assgnm of
  Nothing -> []
  Just (AssgnEq (PAssignmEq (_,_))) -> []
  Just (AssgnPlEq (PAssignmPlus (loc,_))) -> [ErrorChecker loc $ ErrorAssignDecl id]

typeCheckerVariable (PIdent ((l,c), identifier)) tree types currentIdNode = 
  let node = findNodeById currentIdNode tree in case types of
    Infered ->  updateTree (addErrorsNode node [ErrorChecker (l,c) $ NoDecucibleType identifier]) tree
    _ -> case DMap.lookup identifier (getSymbolTable node) of
              Just (_, Variable (varAlreadyDecLine,varAlreadyDecColumn) _ ) -> 
                updateTree (addErrorsNode node [ErrorChecker (varAlreadyDecLine,varAlreadyDecColumn) $ ErrorVarAlreadyDeclared (l,c) identifier]) tree
              Nothing -> updateTree (addEntryNode identifier (Variable (l,c) types) node) tree

typeCheckerDeclExpression environment decExp = case decExp of
  ExprDecArray (ArrayInit _ expression _) -> foldl (typeCheckerDeclArrayExp environment) (DataChecker (Array Infered (0, -1)) []) expression
  ExprDec exp -> typeCheckerExpression environment exp

typeCheckerDeclArrayExp environment types exp = case (typeCheckerDeclExpression environment exp, types) of
  (DataChecker typesFound errorsExp, DataChecker (Array typesInfered (first, n)) errorsTy) -> case sup Sup typesInfered typesFound of
    (Error,compatibility) -> DataChecker (Array typesInfered (first, n + 1)) $ createNewError (errorIncompatibleDeclarationArrayType (getExprDeclPos exp)  typesInfered typesFound exp) compatibility ++ errorsExp ++ errorsTy
    (typesChecked,compatibility) -> DataChecker (Array typesChecked (first, n + 1)) (createNewError (errorIncompatibleDeclarationArrayType (getExprDeclPos exp) typesInfered typesFound exp) compatibility ++ errorsExp ++ errorsTy)  
  (DataChecker _ err1, DataChecker ty err2 ) -> DataChecker ty (err1 ++ err2)

typeCheckerBuildArrayParameter array = case array of
  ArrayDimSingle leftBound _ _ rightBound -> typeCheckerBuildArrayBound False leftBound rightBound
  ArrayDimBound elements -> typeCheckerBuildArrayBound True (ArratBoundConst $ Eint $ PInteger ((-1,-1), "0") ) elements 

typeCheckerBuildArrayBound isNotBounded  lBound rBound = 
  let DataChecker (leftBound,loc1) leftErrors = typeCheckerBuildArrayBound' False lBound
      DataChecker (rightBound,_) rightErrors = typeCheckerBuildArrayBound' True rBound in  
        if leftBound == (-1) ||  rightBound == (-1) 
          then DataChecker (-1, -3) (leftErrors ++ rightErrors)
          else if leftBound > rightBound 
            then DataChecker (leftBound, rightBound) (ErrorChecker loc1 (ErrorBoundsArray leftBound rightBound) :leftErrors ++ rightErrors)
            else DataChecker (leftBound, rightBound) (leftErrors ++ rightErrors)
            where
              typeCheckerBuildArrayBound' isEnd bound = case bound of 
                ArrayBoundIdent (PIdent (loc,name)) -> DataChecker (-1,loc) [ErrorChecker loc $ ErrorDeclarationBoundOnlyConst name]
                ArratBoundConst constant -> case constant of
                  Efloat (PDouble (loc, id)) -> DataChecker (-1,loc) [ErrorChecker loc $ ErrorDeclarationBoundArray Real id]
                  Echar (PChar (loc,id)) -> DataChecker  (-1,loc) [ErrorChecker loc $ ErrorDeclarationBoundArray  Char id]
                  ETrue (PTrue (loc,id)) -> DataChecker (-1,loc) [ErrorChecker loc $ ErrorDeclarationBoundArray  Bool id]
                  EFalse (PFalse (loc,id)) -> DataChecker (-1,loc) [ErrorChecker loc $ ErrorDeclarationBoundArray  Bool id]
                  Estring (PString (loc,id)) -> DataChecker (-1,loc) [ErrorChecker loc $ ErrorDeclarationBoundArray  String id]
                  Eint (PInteger (loc,dimension)) -> 
                    if read dimension == 0  && isNotBounded  &&  isEnd
                      then DataChecker (-1,loc) [ErrorChecker loc ErrorDeclarationCallWithZero]
                      else if isEnd && isNotBounded 
                        then DataChecker (read dimension - 1,loc) [] 
                        else DataChecker (read dimension,loc) []

typeCheckerExpression environment exp = case exp of
    EAss e1 assign e2 -> typeCheckerAssignment environment e1 assign e2
    Eplus e1 (PEplus (loc,_)) e2 -> typeCheckerExpression' environment SupPlus loc e1 e2
    Emod e1 (PEmod (loc,_)) e2 -> typeCheckerExpression' environment SupMod loc e1 e2
    Epow e1 (PEpow (loc,_)) e2 -> typeCheckerExpression' environment SupArith loc e1 e2
    Eminus e1 (PEminus (loc,_)) e2 -> typeCheckerExpression' environment SupMinus loc e1 e2
    Ediv e1 (PEdiv (loc,_)) e2 -> typeCheckerExpression' environment SupArith loc e1 e2
    Etimes e1 (PEtimes (loc,_)) e2 -> typeCheckerExpression' environment SupArith loc e1 e2
    InnerExp _ e _ -> typeCheckerExpression environment e
    Elthen e1 _pElthen e2 -> typeCheckerExpression' environment SupBool (getStartExpPos e1) e1 e2
    Elor e1 _pElor e2 -> typeCheckerExpression' environment SupBool (getStartExpPos e1) e1 e2
    Eland e1 _pEland e2 -> typeCheckerExpression' environment SupBool (getStartExpPos e1) e1 e2
    Eneq e1 _pEneq e2 -> typeCheckerExpression' environment SupBool (getStartExpPos e1) e1 e2
    Eeq e1 _pEeq e2 -> typeCheckerExpression' environment SupBool (getStartExpPos e1) e1 e2
    Egrthen e1 _pEgrthen e2 -> typeCheckerExpression' environment SupBool (getStartExpPos e1) e1 e2
    Ele e1 _pEle e2 -> typeCheckerExpression' environment SupBool (getStartExpPos e1) e1 e2
    Ege e1 _pEge e2 -> typeCheckerExpression' environment SupBool (getStartExpPos e1) e1 e2
    Epreop (Indirection _) e1 -> typeCheckerIndirection environment e1
    Epreop (Address _) e1 ->  typeCheckerAddress environment e1
    Epreop op@(Negation (PNeg _)) e1 ->  typeCheckerExpressionUnary' environment SupBool (getEpreopPos op) e1 Bool
    Epreop op@(MinusUnary (PEminus _)) e1 -> typeCheckerExpressionUnary' environment SupMinus (getEpreopPos op) e1 Int
    Earray expIdentifier arDeclaration -> typeCheckerDeclarationArray  environment expIdentifier arDeclaration
    EFun funidentifier _ passedparams _ -> eFunTypeChecker funidentifier passedparams environment
    Evar identifier -> typeCheckerVariableIdentifiers identifier environment
    Econst (Estring _) -> DataChecker String []
    Econst (Eint _) -> DataChecker Int []
    Econst (Efloat _) -> DataChecker Real []
    Econst (Echar _) -> DataChecker Char []
    Econst (ETrue _) -> DataChecker Bool []
    Econst (EFalse _) -> DataChecker Bool []

typeCheckerVariableIdentifiers identifier environment@(ids,_t,_a) = 
  let errors = concatMap (typeCheckerVariableIdentifier identifier) ids in
    if null errors
    then getVarType identifier environment
    else DataChecker Error errors

typeCheckerVariableIdentifier (PIdent (locExp,idExp)) (_, (idDecl,Variable locDecl _ )) =
  [ErrorChecker locExp  $ ErrorCyclicDeclaration locDecl idDecl | idDecl == idExp]


typeCheckerAssignment environment e1 assign e2 =  
  let DataChecker tyLeft errLeft = typeCheckerLeftExpression assign environment e1
      assOp = getAssignPos assign
      DataChecker tyRight errRight = typeCheckerExpression environment e2 in 
        case assign of
          AssgnEq (PAssignmEq (_,_)) -> 
            let (ty,compatibility) = sup SupDecl tyLeft tyRight
                equalError = createNewError (errorIncompatibleBinaryType assOp e1 e2 tyLeft tyRight) compatibility  in 
                  DataChecker ty (errLeft ++ errRight ++ equalError)
          AssgnPlEq (PAssignmPlus (_,_)) -> 
            let (ty3,compatibilityPlus) = sup SupPlus tyLeft tyRight
                plusError = createNewError (errorIncompatibleAssgnOpType assOp tyLeft e1) compatibilityPlus
                (ty,compatibilityEqual) = sup SupDecl tyLeft ty3 
                equalError = createNewError (errorIncompatibleBinaryType assOp e1 e2 tyLeft tyRight) compatibilityEqual in  
                  DataChecker ty (errLeft ++ errRight ++ plusError ++ equalError)

typeCheckerLeftExpression assign enviroment exp = case exp of
  InnerExp _ exp _ -> typeCheckerExpression enviroment exp
  Evar {} -> typeCheckerExpression enviroment exp
  Earray {} -> typeCheckerExpression enviroment exp
  Epreop {} -> typeCheckerExpression enviroment exp
  _ -> DataChecker Error [ErrorChecker (getStartExpPos exp) $ ErrorNotLeftExpression exp assign]

typeCheckerAddress environment exp = case exp of
  Epreop _ e1 -> let DataChecker ty errors =  typeCheckerIndirection environment e1 in
     DataChecker (Pointer ty) errors
  InnerExp _ e _ -> typeCheckerAddress environment e
  Evar {} -> let DataChecker ty errors = typeCheckerExpression environment exp in DataChecker (Pointer ty) errors
  _ -> DataChecker Error [ErrorChecker (getStartExpPos exp) $ ErrorCantAddressAnExpression exp]

typeCheckerIndirection environment e1 = 
  let newLoc = getStartExpPos e1 in case e1 of
    Earray expIdentifier arDeclaration -> 
      let DataChecker ty errs = typeCheckerDeclarationArray  environment expIdentifier arDeclaration in
        case ty of
          (Pointer ty) -> DataChecker ty errs
          _ -> DataChecker ty (ErrorChecker newLoc (ErrorNoPointerAddress ty e1):errs)
    InnerExp _ e _ -> typeCheckerIndirection environment e
    Epreop (Address _) eAddr -> let DataChecker ty errors = typeCheckerAddress environment eAddr in case ty of
      Pointer tyPoint -> DataChecker tyPoint errors
      ty ->  DataChecker ty $  ErrorChecker newLoc (ErrorNoPointerAddress ty e1):errors
    Epreop (Indirection (PEtimes (loc,_))) ePoint -> let DataChecker ty errors = typeCheckerIndirection environment ePoint in case ty of
      Pointer tyPoint -> DataChecker tyPoint errors
      ty ->  DataChecker ty $  ErrorChecker loc (ErrorNoPointerAddress ty e1):errors
    Evar id ->  let DataChecker ty errors = getVarType id environment in case ty of
      Pointer tyPointed -> DataChecker tyPointed []
      Error -> DataChecker ty errors
      ty -> DataChecker ty $ ErrorChecker newLoc (ErrorNoPointerAddress ty e1):errors
    _ -> DataChecker Error  [ErrorChecker newLoc $ ErrorCantAddressAnExpression e1 ]

eFunTypeChecker funidentifier@(PIdent (loc,identifier)) passedparams environment = 
  case getEntry funidentifier environment of
    DataChecker Nothing errors -> DataChecker Error errors
    DataChecker (Just (Variable _ _ )) _-> DataChecker Error [ErrorChecker loc $ ErrorCalledProcWithVariable identifier]
    DataChecker (Just (Function paramss ty)) _-> 
      let errorss = map (checkPassedParams funidentifier environment (DataChecker 0 []) passedparams) paramss; 
          errors = checkSignatureWithLessError errorss in DataChecker ty errors

checkSignatureWithLessError [err]  = err
checkSignatureWithLessError (err:errorss) = 
  let otherErr = checkSignatureWithLessError errorss in 
    if length otherErr > length err
    then err
    else otherErr

checkPassedParams _  _ (DataChecker _ errors) [] (_,[]) = errors
checkPassedParams (PIdent (loc,identifier)) _ (DataChecker actual errors) [] (_,xs) = ErrorChecker loc (ErrorCalledProcWrongArgs (actual + length xs) actual identifier):errors
checkPassedParams (PIdent (loc,identifier)) _ (DataChecker actual errors) xs (_,[]) =  ErrorChecker loc (ErrorCalledProcWrongArgs actual (length xs + actual) identifier):errors
checkPassedParams funidentifier environment (DataChecker actual errors) (p:passedParams) (locOver,e:expectedParams) =
  let err = checkCorrectTypeOfParam (actual + 1) funidentifier p e environment in 
    checkPassedParams funidentifier environment (DataChecker (actual + 1) (errors ++ err)) passedParams (locOver,expectedParams)

checkCorrectTypeOfParam actual (PIdent (_, identifier)) passedParam (Variable _ tyVar ) environment = case passedParam of
  PassedParWMode mode exp -> checkCorrectTypeOfParamAux (convertMode mode) exp
  PassedPar exp -> checkCorrectTypeOfParamAux Utils.Type.Normal exp
  where
    checkCorrectTypeOfParamAux mode exp = 
      case mode of
        Utils.Type.Ref -> 
          case exp of
            Evar (PIdent (_, identifier)) ->
              let DataChecker tyExp errorsExp = typeCheckerExpression environment exp
                  (ty1,compatibility) = sup SupFun tyVar (convertTyMode mode tyExp) 
                  errorsVar = createNewError (errorIncompatibleTypesChangeToFun (getStartExpPos exp) actual identifier ty1 (Reference tyExp)) compatibility in 
                    errorsVar ++ errorsExp
            _ -> [ErrorChecker (getStartExpPos exp) $ ErrorCantUseExprInARefPassedVar exp]
        Utils.Type.Normal ->
          let DataChecker tyExp errorsExp = typeCheckerExpression environment exp
              (ty1, compatibility) = sup SupFun tyVar (convertTyMode mode tyExp) 
              errorsVar = createNewError (errorIncompatibleTypesChangeToFun (getStartExpPos exp) actual identifier ty1 tyExp) compatibility in 
                errorsVar ++ errorsExp

typeCheckerExpression' environment supMode loc e1 e2 =
  let DataChecker tye1 errors1 = typeCheckerExpression environment e1; 
      DataChecker tye2 errors2 = typeCheckerExpression environment e2;
      (tye,compatibility) = sup supMode tye1 tye2 
      errors = createNewError (errorIncompatibleBinaryType loc e1 e2 tye1 tye2) compatibility in 
        DataChecker tye (errors1 ++ errors2 ++ errors)  

typeCheckerExpressionUnary' environment supMode loc e1 ty =
  let DataChecker tye1 errors1 = typeCheckerExpression environment e1
      (tye,compatibility) = sup supMode tye1 ty 
      errors = createNewError (errorIncompatibleUnaryType loc e1 ty tye1) compatibility in 
        DataChecker tye (errors1 ++ errors)    

typeCheckerDeclarationArray environment exp ardecl@(ArrayInit _ arrays _ ) = case exp of
  InnerExp _ e _ -> typeCheckerDeclarationArray environment e ardecl
  Epreop preop@(Indirection _) _ -> typeCheckerDeclarationArray' "indirection" (getEpreopPos preop) (typeCheckerExpression environment exp) 
  Evar (PIdent (loc, identifier)) -> typeCheckerDeclarationArray' identifier loc (typeCheckerExpression environment exp) 
  _ -> DataChecker Error [ErrorChecker (getStartExpPos exp) $ ErrorArrayCallExpression exp]
  where 
    typeCheckerDeclarationArray' identifier loc dataChecker  = case dataChecker of
      DataChecker types@(Array _ _) expErrors -> let DataChecker dimension errors = getDeclarationDimension environment arrays in 
        case (getArrayDimension types, dimension) of
          (arrayDim, callDim) -> if arrayDim >= callDim 
            then let typeFound = getSubarrayDimension types callDim in if null errors
              then DataChecker typeFound expErrors
              else DataChecker typeFound (errors ++ expErrors)
            else DataChecker Error ([ErrorChecker loc $ ErrorWrongDimensionArray arrayDim callDim identifier] ++ errors ++ expErrors)
      DataChecker types expErrors -> DataChecker Error $ ErrorChecker (getStartExpPos exp) (ErrorDeclarationBoundNotCorrectType types identifier):expErrors
    

getDeclarationDimension _ [] = DataChecker 0 [] 
getDeclarationDimension environment (array:arrays) = let DataChecker dimension errors = getDeclarationDimension environment arrays in 
  case array of
    ExprDec exp -> 
      let DataChecker ty expErrors = typeCheckerExpression environment exp 
          (_,compatibility) = sup SupDecl Int ty 
          errorsFound = createNewError (errorIncompatibleTypeArrayIndex (getStartExpPos exp) ty exp )  compatibility in
            DataChecker (dimension + 1) (errors ++ errorsFound ++ expErrors)
    expDecl -> DataChecker (dimension + 1) $ ErrorChecker (getExprDeclPos expDecl) (ErrorArrayExpressionRequest expDecl) : errors


-- funzione di typechecking che controlla se ci sono return nelle funzioni, 
-- e che i return siano in tutti i path del codice
typeCheckerReturns (Node _ _ _ startnodes) = concatMap typeCheckerReturnPresenceFun startnodes

typeCheckerReturnPresenceFun :: BPTree BP -> [ErrorChecker]
typeCheckerReturnPresenceFun ((Node (funname,(locstart,locend)) (BP _ rets _ (ProcedureBlk tyret)) _ children)) = 
  case tyret of
    Utils.Type.Void ->           
      let declFunsInChildren = filter isNodeAProc children in
        concatMap typeCheckerReturnPresenceFun declFunsInChildren
    _ -> 
      if null rets
        then
          typeCheckerReturnPresence children funname (locstart,locend)
        else
          -- ho quasi finito: 
          -- devo controllare se ci sono altri blocchi funzione nei children o dopo 
          -- e verificare anche quelli
          let declFunsInChildren = filter isNodeAProc children;        
          in
            concatMap typeCheckerReturnPresenceFun declFunsInChildren

typeCheckerReturnPresenceElse (Node _id (BP _ rets _ IfElseBlk) _ children) funname (locstart, locend) =
  if null rets
    then typeCheckerReturnPresence children funname (locstart, locend)
    else []

getblkStartEndPos (Node (_,(act_locstart, act_locend)) (BP _ _rets _ _blocktype) _ _children) = (act_locstart, act_locend)

isNodeAProc (Node (_,(_act_locstart, _act_locend)) (BP _ _rets _ blocktype) _ _children) = case blocktype of
   ProcedureBlk _ -> True
   _ -> False

continueCheckerRetPresenceOnSubProcs children xs = 
  let declFunsInChildren = filter isNodeAProc children;
      declFunsAhead = filter isNodeAProc xs
  in
    let childrens = concatMap typeCheckerReturnPresenceFun declFunsInChildren
        aheads = concatMap typeCheckerReturnPresenceFun declFunsAhead in
        childrens ++ aheads

typeCheckerReturnPresence [] funname (locstart, _locend) = [ErrorChecker locstart $ ErrorFunctionWithNotEnoughReturns funname]

typeCheckerReturnPresence ((Node (_,(act_locstart, act_locend)) (BP _ rets _ blocktype) _ children):xs) funname (locstart, _locend) =
  case blocktype of
    IfSimpleBlk -> 
      if null xs
        then [ErrorChecker act_locstart $ ErrorFunctionWithNotEnoughReturns funname]
        else typeCheckerReturnPresence xs funname (act_locstart, act_locend)
    IfThenBlk -> 
      if null rets
        then 
          let errs1 = typeCheckerReturnPresence children funname (act_locstart, act_locend);
              elset = typeCheckerReturnPresenceElse (head xs) funname (getblkStartEndPos (head xs)) in
                case (null errs1,null elset) of
                  (True,True) ->
                    -- ho quasi finito: ho trovato un return sia nell'if che nell'else, quindi questo blocco è ok
                    -- devo però controllare se ci sono altri blocchi funzione nei children o dopo 
                    -- e verificare anche quelli. Dato che lo devo fare molte volte ho
                    -- creato una funzione apposta: continueCheckerRetPresenceOnSubProcs
                    continueCheckerRetPresenceOnSubProcs children xs
                  _otherwhise -> 
                    -- sono nel caso in cui ho trovato un return nel then o nell'else o da nessuna parte,
                    -- non posso ancora affermare che ci siano errori, 
                    -- in quanto potrei avere dei blocchi if else successivi 
                    -- dove ci sono i return, ad esempio.
                    -- devo andare avanti con la computazione
                    if null (tail xs)
                      then [ErrorChecker act_locstart $ ErrorFunctionWithNotEnoughReturns funname]
                      else typeCheckerReturnPresence (tail xs) funname (getblkStartEndPos (head (tail xs)))
        else
          let elset = typeCheckerReturnPresenceElse (head xs) funname (getblkStartEndPos (head xs)) in
            case null elset of
              True -> continueCheckerRetPresenceOnSubProcs children xs
              _oth ->
                if null (tail xs)
                  then [ErrorChecker act_locstart $ ErrorFunctionWithNotEnoughReturns funname]
                  else typeCheckerReturnPresence (tail xs) funname (getblkStartEndPos (head (tail xs)))

    ProcedureBlk tyret -> 
      case tyret of
        Utils.Type.Void -> continueCheckerRetPresenceOnSubProcs children xs
        _ -> 
          if null rets -- se non ci sono return al blocco base
            then
              let c = typeCheckerReturnPresence children funname (act_locstart, act_locend) in
                case null c of
                  True -> continueCheckerRetPresenceOnSubProcs children xs
                  _oth -> [ErrorChecker act_locstart $ ErrorFunctionWithNotEnoughReturns funname]
            else continueCheckerRetPresenceOnSubProcs children xs
    WhileBlk -> 
      if null xs
        then [ErrorChecker act_locstart $ ErrorFunctionWithNotEnoughReturns funname]
        else typeCheckerReturnPresence xs funname (act_locstart, act_locend)
    DoWhileBlk -> 
      if null rets
        then
          let errs = typeCheckerReturnPresence children funname (act_locstart, act_locend) in
          case null errs of
            True -> continueCheckerRetPresenceOnSubProcs children xs
            _otherwhise ->
              if null xs
                then [ErrorChecker locstart $ ErrorFunctionWithNotEnoughReturns funname]
                else typeCheckerReturnPresence xs funname (act_locstart, act_locend)
        else continueCheckerRetPresenceOnSubProcs children xs
    SimpleBlk -> 
      if null rets
        then
          typeCheckerReturnPresence children funname (act_locstart, act_locend)
        else continueCheckerRetPresenceOnSubProcs children xs
