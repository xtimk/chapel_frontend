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

typeCheckerFunction (FunDec (PProc (loc@(l,c),_) ) signature body@(BodyBlock  (POpenGraph (locGraph,_)) _ _)) = do
  typeCheckerSignature loc locGraph signature
  typeCheckerBody ProcedureBlk (createId' l c (getFunName signature)) body
  get  

typeCheckerSignature locstart locEnd signature = case signature of
  SignNoRet identifier (FunParams _ params _) -> typeCheckerSignature' locstart locEnd identifier params Utils.Type.Void
  SignWRet identifier (FunParams _ params _) _ types -> typeCheckerSignature' locstart locEnd identifier params (convertTypeSpecToTypeInferred types)
  SignWArRet identifier (FunParams _ params _) _ array types -> do
    environment <- get
    let DataChecker typeArray errors = typeCheckerBuildArrayType environment array (convertTypeSpecToTypeInferred types)
    modify $ addErrorsCurrentNode errors
    typeCheckerSignature' locstart locEnd identifier params typeArray

typeCheckerSignature' locstart locEnd ident@(PIdent (loc,identifier)) params types = 
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
            let errorsOverloading = typeCheckerSignatureOverloading identifier loc variables varOfvar
            let DataChecker _ errorsReturnType = sup SupFun identifier loc types t
            let convertErrors = map (errorConvertOVerloadingReturn locstart locEnd) errorsReturnType
            if null errors 
            then do
              put (symtable, updateTree (modFunAddOverloading identifier loc variables node) tree, currentIdNode )
              modify $ addErrorsCurrentNode convertErrors
              get
            else do
              modify $ addErrorsCurrentNode (errorsOverloading ++ convertErrors)
              get 

typeCheckerSignatureOverloading _ _ _ [] = []
typeCheckerSignatureOverloading identifier loc var ((locFun,x):xs) = case (var,x) of
  ([],[]) -> [ErrorChecker loc $ ErrorSignatureAlreadyDeclared locFun identifier]
  ([],_) -> typeCheckerSignatureOverloading identifier loc var xs
  _ -> let errors = typeCheckerSignatureOverloading' identifier loc locFun var x in 
    if null errors
    then typeCheckerSignatureOverloading identifier loc var xs
    else errors

typeCheckerSignatureOverloading' identifier loc locFun [] [] = [ErrorChecker loc $ ErrorSignatureAlreadyDeclared locFun identifier]
typeCheckerSignatureOverloading' _ _ _ [] _ = []
typeCheckerSignatureOverloading' _ _ _ _ [] = []
typeCheckerSignatureOverloading' identifier loc locFun ((Variable _ ty1):xs) ((Variable _ ty2):ys) = 
  if show ty1 == show ty2
  then typeCheckerSignatureOverloading' identifier loc locFun xs ys
  else []
  
typeCheckerParams xs = do
   mapM_ typeCheckerParam xs
   get

typeCheckerParam x = case x of
  ParNoMode identifiers _ types ->  do
    mapM_ (typeCheckerParam' Normal types) identifiers
    get
  ParWMode (RefMode mode) identifiers  _ types ->  do
    mapM_ (typeCheckerParam' (convertMode mode) types) identifiers
    get

typeCheckerParam' mode types identifier = 
  let typefound = convertTypeSpecToTypeInferred types in do 
    modify (typeCheckerParam'' identifier mode typefound)
    get

typeCheckerParam'' (PIdent (loc,identifier)) mode ty env@(sym,_t,_i) = case typeCheckerParam''' sym identifier of
  Just (_,(_, Variable (varAlreadyDecLine,varAlreadyDecColumn) _)) -> 
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
        modify $ addErrorsCurrentNode [ ErrorChecker (getExpPos exp) $ ErrorOnlyRightExpression exp]
        get
  ret@(RetVal return exp _semicolon) -> do
    (_s,tree,current_id) <- get
    let DataChecker tyret errs2 = typeCheckerExpression (_s,tree,current_id) exp 
    modify $ addErrorsCurrentNode errs2
    typeCheckerReturn return tyret
    modify $ addStatementCurrentNode ret
    get

  ret@(RetVoid return _semicolon) -> do
    typeCheckerReturn return Utils.Type.Void
    modify $ addStatementCurrentNode ret
    get

typeCheckerReturn (PReturn (pos, _ret)) tyret = do 
  (_s,tree,current_id) <- get
  case findProcedureGetBlkType tree current_id of
    ProcedureBlk -> do
      let DataChecker tyfun errs1 = getFunRetType (_s,tree,current_id);
      let DataChecker _ errs = sup SupRet (getFunNameFromEnv (_s,tree,current_id)) pos tyret tyfun 
      let errors = errs1 ++ errs 
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
    modify $ addErrorsCurrentNode  [ErrorChecker (getExpPos expression) ErrorGuardNotBoolean]
    get

typeCheckerDeclaration x = do
  environment <- get
  case x of
    NoAssgmDec ids _colon types -> typeCheckerIdentifiers ids Nothing (convertTypeSpecToTypeInferred types) Infered
    AssgmDec ids assignment exp -> do
      let DataChecker ty errors = typeCheckerDeclExpression environment exp
      typeCheckerIdentifiers ids (Just assignment) Infered ty
      modify $ addErrorsCurrentNode errors
      get
    AssgmTypeDec ids _colon types assignment exp -> do
      let DataChecker ty errors = typeCheckerDeclExpression environment exp
      typeCheckerIdentifiers ids (Just assignment) (convertTypeSpecToTypeInferred types) ty
      modify $ addErrorsCurrentNode errors
      get
    NoAssgmArrayDec ids _colon array types -> typeCheckerIdentifiersArray ids  Nothing array (convertTypeSpecToTypeInferred types) Infered
    AssgmArrayTypeDec ids _colon array types assignment exp -> do
      let DataChecker ty errors = typeCheckerDeclExpression environment exp
      typeCheckerIdentifiersArray ids (Just assignment) array (convertTypeSpecToTypeInferred types) ty
      modify $ addErrorsCurrentNode errors
      get
    AssgmArrayDec ids _colon array assignment exp -> do
      let DataChecker ty errors = typeCheckerDeclExpression environment exp
      typeCheckerIdentifiersArray ids (Just assignment) array Infered ty
      modify $ addErrorsCurrentNode errors
      get

typeCheckerIdentifiersArray ids assgn array typeLeft typeRight = do
  environment <- get
  let DataChecker typeArray errors = typeCheckerBuildArrayType environment array typeLeft
  modify (addErrorsCurrentNode errors)
  typeCheckerIdentifiers ids assgn typeArray typeRight
  get

typeCheckerBuildArrayType environment (ArrayDeclIndex _ array _) = typeCheckerBuildArrayType' environment array
  where
    typeCheckerBuildArrayType' _ [] types = DataChecker types []
    typeCheckerBuildArrayType' enviroment (array:arrays) types = 
      let DataChecker bounds errors = typeCheckerBuildArrayParameter enviroment array; DataChecker typesFound othersErrors = typeCheckerBuildArrayType' enviroment arrays types in 
        DataChecker (Array typesFound bounds) (errors ++ othersErrors)

typeCheckerIdentifiers identifiers assgn typeLeft typeRight = do
  mapM_ (typeCheckerIdentifier assgn typeLeft typeRight) identifiers
  get

typeCheckerIdentifier assgn typeLeft typeRight id@(PIdent (loc, identifier)) = do
  (_s,_,current_id) <- get
  let DataChecker ty errors = sup SupDecl identifier loc typeLeft typeRight in do
    modify $ addErrorsCurrentNode (errors ++ typeCheckerAssignmentDecl assgn)
    case ty of
      Error -> get
      _ -> do
        modify (\(_s,tree,_i) -> (_s, typeCheckerVariable id tree ty current_id, _i ))
        get

typeCheckerAssignmentDecl assgnm = case assgnm of
  Nothing -> []
  Just (AssgnEq (PAssignmEq (_,_))) -> []
  Just (AssgnPlEq (PAssignmPlus (loc,_))) -> [ErrorChecker loc ErrorAssignDecl]

typeCheckerVariable (PIdent ((l,c), identifier)) tree types currentIdNode = 
  let node = findNodeById currentIdNode tree in case types of
    Infered ->  updateTree (addErrorsNode node [ErrorChecker (l,c) $ NoDecucibleType identifier]) tree
    _ -> case DMap.lookup identifier (getSymbolTable node) of
              Just (_, Variable (varAlreadyDecLine,varAlreadyDecColumn) _) -> 
                updateTree (addErrorsNode node [ErrorChecker (varAlreadyDecLine,varAlreadyDecColumn) $ ErrorVarAlreadyDeclared (l,c) identifier]) tree
              Nothing -> updateTree (addEntryNode identifier (Variable (l,c) types) node) tree

typeCheckerDeclExpression environment decExp = case decExp of
  ExprDecArray (ArrayInit _ expression _) -> foldl (typeCheckerDeclArrayExp environment) (DataChecker (Array Infered (Fix 0, Fix 0)) []) expression
  ExprDec exp -> typeCheckerExpression environment exp

typeCheckerDeclArrayExp environment types expression = case (typeCheckerDeclExpression environment expression, types) of
  (DataChecker typesFound errorsExp, DataChecker (Array typesInfered (first, Fix n)) errorsTy) -> case sup SupDecl "array" (getExprDeclPos expression) typesInfered typesFound of
    DataChecker Error e -> DataChecker (Array typesInfered (first, Fix $ n + 1)) $ modErrorsPos (getExprDeclPos expression) e ++ errorsExp ++ errorsTy
    DataChecker typesChecked e -> DataChecker (Array typesChecked (first, Fix $ n + 1)) (e ++ errorsExp ++ errorsTy)  
  (DataChecker _ err1, DataChecker ty err2 ) -> DataChecker ty (err1 ++ err2)

typeCheckerBuildArrayParameter enviroment array = case array of
  ArrayDimSingle leftBound _ _ rightBound -> typeCheckerBuildArrayBound enviroment leftBound rightBound
  ArrayDimBound elements -> typeCheckerBuildArrayBound enviroment (ArratBoundConst $ Eint $ PInteger ((-1,-1), "0") ) elements 

typeCheckerBuildArrayBound enviroment lBound rBound = 
  let DataChecker leftBound leftErrors = typeCheckerBuildArrayBound' lBound; 
      DataChecker rightBound rightErrors = typeCheckerBuildArrayBound' rBound in  
         DataChecker (leftBound, rightBound) (leftErrors ++ rightErrors)
            where
              typeCheckerBuildArrayBound' bound = case bound of 
                ArrayBoundIdent id@(PIdent (loc,name)) -> case getVarType id enviroment of 
                  DataChecker Error err -> DataChecker (Fix $ -1) (modErrorsPos loc err)
                  DataChecker Int _-> DataChecker (Var name) []
                  DataChecker types _-> DataChecker (Fix $ -1) [ErrorChecker loc $ ErrorDeclarationBoundNotCorrectType types name]
                ArratBoundConst constant -> case constant of
                  Efloat (PDouble (loc, id)) -> DataChecker (Fix $ -1) [ErrorChecker loc $ ErrorDeclarationBoundArray Real id]
                  Echar (PChar (loc,id)) -> DataChecker (Fix $ -1) [ErrorChecker loc $ ErrorDeclarationBoundArray  Char id]
                  ETrue (PTrue (loc,id)) -> DataChecker (Fix $ -1) [ErrorChecker loc $ ErrorDeclarationBoundArray  Bool id]
                  EFalse (PFalse (loc,id)) -> DataChecker (Fix $ -1) [ErrorChecker loc $ ErrorDeclarationBoundArray  Bool id]
                  Estring (PString (loc,id)) -> DataChecker (Fix $ -1) [ErrorChecker loc $ ErrorDeclarationBoundArray  String id]
                  Eint (PInteger (_,dimension)) -> DataChecker (Fix $ read dimension) []

typeCheckerExpression environment exp = case exp of
    EAss e1 assign e2 -> typeCheckerAssignment environment e1 assign e2
    Eplus e1 (PEplus (loc,_)) e2 -> typeCheckerExpression' environment SupMinus loc e1 e2
    Emod e1 (PEmod (loc,_)) e2 -> typeCheckerExpression' environment SupMod loc e1 e2
    Eminus e1 (PEminus (loc,_)) e2 -> typeCheckerExpression' environment SupArith loc e1 e2
    Ediv e1 (PEdiv (loc,_)) e2 -> typeCheckerExpression' environment SupArith loc e1 e2
    Etimes e1 (PEtimes (loc,_)) e2 -> typeCheckerExpression' environment SupArith loc e1 e2
    InnerExp _ e _ -> typeCheckerExpression environment e
    Elthen e1 _pElthen e2 -> typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Elor e1 _pElor e2 -> typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Eland e1 _pEland e2 -> typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Eneq e1 _pEneq e2 -> typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Eeq e1 _pEeq e2 -> typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Egrthen e1 _pEgrthen e2 -> typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Ele e1 _pEle e2 -> typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Ege e1 _pEge e2 -> typeCheckerExpression' environment SupBool (getExpPos e1) e1 e2
    Epreop (Indirection _) e1 -> typeCheckerIndirection environment e1
    Epreop (Address _) e1 ->  typeCheckerAddress environment e1
    Epreop (Negation (PNeg (loc,_))) e1 ->  typeCheckerExpression' environment SupBool (getExpPos e1) e1 (Econst (ETrue (PTrue (loc,"true"))))
    Earray expIdentifier arDeclaration -> typeCheckerDeclarationArray  environment expIdentifier arDeclaration
    EFun funidentifier _ passedparams _ -> eFunTypeChecker funidentifier passedparams environment
    Evar identifier -> getVarType identifier environment
    Econst (Estring _) -> DataChecker String []
    Econst (Eint _) -> DataChecker Int []
    Econst (Efloat _) -> DataChecker Real []
    Econst (Echar _) -> DataChecker Char []
    Econst (ETrue _) -> DataChecker Bool []
    Econst (EFalse _) -> DataChecker Bool []

typeCheckerAssignment environment e1 assign e2 =  
  let DataChecker tyLeft errLeft = typeCheckerLeftExpression assign environment e1;
      DataChecker tyRight errRight = typeCheckerExpression environment e2 in 
        case assign of
          AssgnEq (PAssignmEq (_,_)) -> 
            let DataChecker ty errors = sup SupDecl "" (getExpPos e1) tyLeft tyRight; in 
                  DataChecker ty (errLeft ++ errRight ++ errors)
          AssgnPlEq (PAssignmPlus (_,_)) ->  
            let DataChecker ty3 errors3 = sup SupPlus "" (getExpPos e1) tyLeft tyRight;
                DataChecker ty errors = sup SupDecl "" (getExpPos e1) tyLeft ty3 in  
                  DataChecker ty (errLeft ++ errRight ++ errors3 ++ errors)

typeCheckerLeftExpression assign enviroment exp = case exp of
  Evar {} -> typeCheckerExpression enviroment exp
  Earray {} -> typeCheckerExpression enviroment exp
  Epreop {} -> typeCheckerExpression enviroment exp
  _ -> DataChecker Error [ErrorChecker (getExpPos exp) $ ErrorNotLeftExpression exp assign]

typeCheckerAddress enviroment exp = case exp of
  Evar {} -> let DataChecker ty errors = typeCheckerExpression enviroment exp in DataChecker (Pointer ty) errors
  _ -> DataChecker Error [ErrorChecker (getExpPos exp) ErrorCantAddressAnExpression]

typeCheckerIndirection environment e1 = 
  let newLoc = getExpPos e1 in case e1 of
    Epreop (Indirection (PEtimes (loc,_))) e1 -> let DataChecker ty errors = typeCheckerIndirection environment e1 in case ty of
      Pointer tyPoint -> DataChecker tyPoint errors
      ty ->  DataChecker ty $  ErrorChecker loc (ErrorNoPointerAddress ty "expression"):errors
    Evar id@(PIdent (_,identifier)) ->  let DataChecker ty errors = getVarType id environment; in case ty of
      Pointer tyPointed -> DataChecker tyPointed []
      Error -> DataChecker ty errors
      ty -> DataChecker ty $ ErrorChecker newLoc (ErrorNoPointerAddress ty identifier):errors
    _ -> DataChecker Error  [ErrorChecker newLoc ErrorCantAddressAnExpression]

eFunTypeChecker funidentifier@(PIdent (loc,identifier)) passedparams environment = 
  case getEntry funidentifier environment of
    DataChecker Nothing errors -> DataChecker Error errors
    DataChecker (Just (Variable _ _)) _-> DataChecker Error [ErrorChecker loc $ ErrorCalledProcWithVariable identifier]
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

checkCorrectTypeOfParam actual (PIdent (_, identifier)) (PassedPar exp) (Variable _ tyVar) environment = 
  let DataChecker tyExp errorsExp = typeCheckerExpression environment exp;
      DataChecker _ errorsVar = sup SupFun "" (getExpPos exp) tyExp tyVar in errorsIncompatibleTypesChangeToFun actual identifier (errorsVar ++ errorsExp)

errorsIncompatibleTypesChangeToFun pos id = map (errorIncompatibleTypesChangeToFun pos id)

errorIncompatibleTypesChangeToFun pos id (ErrorChecker loc (ErrorIncompatibleDeclTypes _ ty1 ty2)) = 
  ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam pos ty1 ty2 id
errorIncompatibleTypesChangeToFun _ _ error = error

typeCheckerExpression' environment supMode loc e1 e2 =
  let DataChecker tye1 errors1 = typeCheckerExpression environment e1; 
      DataChecker tye2 errors2 = typeCheckerExpression environment e2;
      DataChecker tye errors = sup supMode "" loc tye1 tye2 in 
      DataChecker tye (errors1 ++ errors2 ++ errors)    

typeCheckerDeclarationArray environment exp (ArrayInit _ arrays _ ) = case exp of
  Evar (PIdent ((l,c), identifier)) -> case typeCheckerExpression environment exp of
    DataChecker types@(Array _ _) expErrors -> let DataChecker dimension errors = getDeclarationDimension environment arrays in 
      case (getArrayDimension types, dimension) of
        (arrayDim, callDim) -> if arrayDim >= callDim 
          then let typeFound = getSubarrayDimension types callDim in if null errors
            then DataChecker typeFound []
            else DataChecker typeFound (errors ++ expErrors)
          else DataChecker Error ([ErrorChecker (l,c) $ ErrorWrongDimensionArray arrayDim callDim identifier] ++ errors ++ expErrors)
    DataChecker types expErrors -> DataChecker Error $ ErrorChecker (getExpPos exp) (ErrorDeclarationBoundNotCorrectType types identifier):expErrors
  _ -> DataChecker Error [ErrorChecker (getExpPos exp) ErrorArrayCallExpression]

getDeclarationDimension _ [] = DataChecker 0 [] 
getDeclarationDimension environment (array:arrays) = let DataChecker dimension errors = getDeclarationDimension environment arrays in 
  case array of
    ExprDec exp -> let DataChecker ty expErrors = typeCheckerExpression environment exp in case sup Sup "" (getExpPos exp) Int ty of
      DataChecker Error errorsFound -> DataChecker (dimension + 1) (errors ++ errorsFound ++ expErrors)
      _ -> DataChecker (dimension + 1) (errors ++ expErrors)
    expDecl -> DataChecker (dimension + 1) $ ErrorChecker (getExprDeclPos expDecl) ErrorArrayExpressionRequest : errors


-- funzione di typechecking che controlla se ci sono return nelle funzioni, 
-- e che i return siano in tutti i path del codice
typeCheckerReturns bp@(Node _ _ _ startnodes) = concat (map typeCheckerReturnPresenceFun startnodes)

typeCheckerReturnPresenceFun :: BPTree BP -> [ErrorChecker]
typeCheckerReturnPresenceFun (node@(Node (funname,(locstart,locend)) (BP _ rets _ ProcedureBlk) _ (children))) = 
  if null rets
    then
      typeCheckerReturnPresence children funname (locstart,locend)
    else
      -- ho quasi finito: 
      -- devo controllare se ci sono altri blocchi funzione nei children o dopo 
      -- e verificare anche quelli
      let declFunsInChildren = filter isNodeAProc children;        
      in
        concat (map typeCheckerReturnPresenceFun declFunsInChildren)

typeCheckerReturnPresenceElse node@(Node id (BP _ rets _ IfElseBlk) _ (children)) funname (locstart, locend) =
  if null rets
    then typeCheckerReturnPresence children funname (locstart, locend)
    else []

getblkStartEndPos (Node (_,(act_locstart, act_locend)) (BP _ rets _ blocktype) _ (children)) = (act_locstart, act_locend)

isNodeAProc node@(Node (_,(act_locstart, act_locend)) (BP _ rets _ blocktype) _ (children)) = blocktype == ProcedureBlk

continueCheckerRetPresenceOnSubProcs children xs = 
  let declFunsInChildren = filter isNodeAProc children;
      declFunsAhead = filter isNodeAProc xs
  in
    let childrens = concat (map typeCheckerReturnPresenceFun declFunsInChildren)
        aheads = concat (map typeCheckerReturnPresenceFun declFunsAhead) in
        childrens ++ aheads

typeCheckerReturnPresence [] funname (locstart, locend) = [ErrorChecker locstart $ ErrorFunctionWithNotEnoughReturns funname]

typeCheckerReturnPresence (node@(Node (_,(act_locstart, act_locend)) (BP _ rets _ blocktype) _ (children)):xs) funname (locstart, locend) =
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
                case ((null errs1),(null elset)) of
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
                      else (typeCheckerReturnPresence (tail xs) funname (getblkStartEndPos (head (tail xs))))
        else
          let elset = typeCheckerReturnPresenceElse (head xs) funname (getblkStartEndPos (head xs)) in
            case null elset of
              True -> continueCheckerRetPresenceOnSubProcs children xs
              _oth ->
                if null (tail xs)
                  then [ErrorChecker act_locstart $ ErrorFunctionWithNotEnoughReturns funname]
                  else (typeCheckerReturnPresence (tail xs) funname (getblkStartEndPos (head (tail xs))))

    ProcedureBlk -> 
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