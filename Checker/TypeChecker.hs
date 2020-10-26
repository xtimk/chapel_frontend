module Checker.TypeChecker where

import Control.Monad.Trans.State
import qualified Data.Map as DMap
import AbsChapel
import Checker.SymbolTable
import Checker.BPTree
import Data.Maybe
import Debug.Trace

startState = (DMap.empty, [], Checker.BPTree.Node {Checker.BPTree.id = "0", val = BP {symboltable = DMap.empty, statements = [], errors = [], blocktype = ExternalBlk}, parentID = Nothing, children = []}, "0")

typeChecker (Progr p) = typeCheckerModule p

typeCheckerModule (Mod m) = typeCheckerExt m

createId l c name = show l ++ "_" ++ show c ++ "_" ++ name

typeCheckerExt [] = get

typeCheckerExt (x:xs) = case x of
    ExtDecl (Decl decMode declList _ ) -> do
        mapM_ typeCheckerDeclaration declList
        typeCheckerExt xs
    ExtFun (FunDec (PProc ((l,c),funname) ) signature body) -> do
        typeCheckerSignature signature
        typeCheckerBody ProcedureBlk (createId l c funname) body
        typeCheckerExt xs

typeCheckerSignature signature = case signature of
  SignNoRet identifier (FunParams _ params _) -> typeCheckerSignature' identifier params Infered
  SignWRet identifier (FunParams _ params _) _ types -> typeCheckerSignature' identifier params (convertTypeSpecToTypeInferred types)

typeCheckerSignature' (PIdent ((line,column),identifier)) params types = 
  case types of
    Error _ -> get
    _ -> do
      typeCheckerParams params
      (symtable, _e, tree, currentIdNode) <- get
      let node = findNodeById currentIdNode tree; variables = map (snd.snd) (DMap.toAscList symtable)  in
        put (symtable, _e, updateTree (addEntryNode identifier (Function (line,column) variables types) node) tree, currentIdNode )
      get 

typeCheckerSignatureParams identifier = get
  
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

typeCheckerParam' mode types (PIdent ((line,column),identifier)) = 
  let typefound = convertTypeSpecToTypeInferred types in 
    modify(\(sym,_e,_t,_i) -> (DMap.insert identifier (identifier, Variable mode (line,column) typefound) sym,_e,_t,_i))

  
typeCheckerBody blkType identifier (BodyBlock  _ xs _  ) = do
  -- create new child for the blk and enter in it
  (sym, _e, tree, current_id) <- get
  let actualNode = findNodeById current_id tree; child = createChild identifier blkType actualNode in
     put (DMap.empty, _e, addChild actualNode (setSymbolTable sym child) tree, identifier)
  -- process body
  mapM_ (typeCheckerBody' blkType) xs
  modify(\(_s,_e,_t,_) -> (_s,_e,_t,current_id))
  get

typeCheckerBody' blkType x = do
  case x of
    Stm statement -> typeCheckerStatement statement
    Fun _ _ -> get
    DeclStm (Decl decMode declList _ ) -> do
      mapM_ typeCheckerDeclaration declList
      get
    Block body@(BodyBlock (POpenGraph ((l,c), name)) _ _) -> typeCheckerBody SimpleBlk (createId l c name) body
  get

typeCheckerStatement statement = case statement of
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
  StExp exp@(EAss e1 eqsym e2) _semicolon ->
    if isExpVar e1
      then do
        env <- get
        case typeCheckerExpression env (EAss e1 eqsym e2) of
          DataChecker (Error _ )error -> do
            (_s,_e,tree,current_id) <- get
            let node = findNodeById current_id tree in
              modify (\(_s, _e, tree,_i) -> (_s, _e, (updateTree (addErrorsNode node error) tree) , _i ))
            get
          _ -> get -- se non ho errore non faccio nulla nello stato
      else get
  StExp _ _-> get
  RetVal _return exp _semicolon -> do
    (_s,_e,tree,current_id) <- get
    case getBlkType tree current_id of
      ProcedureBlk -> get -- devo controllare quì che il tipo di ritorno nel return sia compatibile con il tipo di ritorno della funzione
      _otherwhise -> do
        get
        let node = findNodeById current_id tree ; errors = ErrorChecker (getExpPos exp) ErrorReturnNotInsideAProcedure in
          modify (\(_s, _e, tree,_i) -> (_s, _e, (updateTree (addErrorNode errors node) tree) , _i ))
        get
    get
  RetVoid {} -> get

isExpVar e = case e of
  (Evar (PIdent ((l,c),indentifier))) -> True
  _otherwhise -> False

getVarPos (Evar (PIdent ((l,c),indentifier))) = (l,c)

getVarId (Evar (PIdent ((l,c),indentifier))) = indentifier

getAssignOpPos op = case op of
  (AssgnEq (PAssignmEq ((l,c),_))) -> (l,c)
  (AssgnPlEq (PAssignmPlus ((l,c),_))) -> (l,c)

getAssignOpTok op = case op of
  (AssgnEq (PAssignmEq ((l,c),t))) -> t
  (AssgnPlEq (PAssignmPlus ((l,c),t))) -> t

typeCheckerGuard (SGuard _ expression _) = do
  (_s,_e,tree,current_id) <- get
  case typeCheckerExpression (_s,_e,tree,current_id) expression of
    DataChecker Bool errors-> get -- questo dovrebbe essere l'unico caso accettato, se expression non e' bool devo segnalarlo come errore
    _otherwhise -> do
      let node = findNodeById current_id tree in
        modify (\(_s, _e, tree,_i) -> (_s, _e, (updateTree (addErrorNode (ErrorChecker (getExpPos expression) ErrorGuardNotBoolean ) node) tree) , _i ))
      get
      

typeCheckerDeclaration x = do
  environment <- get
  case x of
    NoAssgmDec ids _colon types -> typeCheckerIdentifiers ids (convertTypeSpecToTypeInferred types) Infered
    AssgmDec ids assigment exp -> let DataChecker ty errors = typeCheckerDeclExpression environment exp in do
      typeCheckerIdentifiers ids Infered ty
      modify $ addErrorsCurrentNode errors
      get
    AssgmTypeDec ids _colon types assignment exp -> let DataChecker ty errors = typeCheckerDeclExpression environment exp in do
       typeCheckerIdentifiers ids (convertTypeSpecToTypeInferred types) ty
       modify $ addErrorsCurrentNode errors
       get
    NoAssgmArrayFixDec ids _colon array -> typeCheckerIdentifiersArray ids array Infered Infered
    NoAssgmArrayDec ids _colon array types -> typeCheckerIdentifiersArray ids array (convertTypeSpecToTypeInferred types) Infered
    AssgmArrayTypeDec ids _colon array types assignment exp ->  let DataChecker ty errors = typeCheckerDeclExpression environment exp in do
      typeCheckerIdentifiersArray ids array (convertTypeSpecToTypeInferred types) ty
      modify $ addErrorsCurrentNode errors
      get
    AssgmArrayDec ids _colon array assignment exp ->  let DataChecker ty errors = typeCheckerDeclExpression environment exp in do
       typeCheckerIdentifiersArray ids array Infered ty
       modify $ addErrorsCurrentNode errors
       get

typeCheckerIdentifiersArray ids array typeLeft typeRight = do
  environment <- get
  let DataChecker typeArray errors = typeCheckerBuildArrayType environment array typeLeft in do
    modify (addErrorsCurrentNode errors)
    typeCheckerIdentifiers ids typeArray typeRight
  get

typeCheckerBuildArrayType environment (ArrayDeclIndex _ array _) = typeCheckerBuildArrayType' environment array
  where
    typeCheckerBuildArrayType' enviroment [] types = DataChecker types []
    typeCheckerBuildArrayType' enviroment (array:arrays) types = 
      let DataChecker bounds errors = typeCheckerBuildArrayParameter enviroment array; DataChecker typesFound othersErrors = typeCheckerBuildArrayType' enviroment arrays types in 
        DataChecker (Array typesFound bounds) (errors ++ othersErrors)

typeCheckerIdentifiers identifiers typeLeft typeRight = do
  mapM_ (typeCheckerIdentifier typeLeft typeRight) identifiers
  get

typeCheckerIdentifier typeLeft typeRight id@(PIdent (loc, identifier)) = do
  (_s,_e,tree,current_id) <- get
  let DataChecker ty errors = supdecl identifier loc typeLeft typeRight in do
    modify $ addErrorsCurrentNode errors
    case ty of
      Error _ -> get
      _ -> do
        modify (\(_s,_e,tree,_i) -> (_s, _e, typeCheckerVariable id tree ty current_id, _i ))
        get

typeCheckerVariable (PIdent ((l,c), identifier)) tree types currentIdNode = 
  let node = findNodeById currentIdNode tree in case DMap.lookup identifier (getSymbolTable node) of
      Just (varAlreadyDeclName, Variable _ (varAlreadyDecLine,varAlreadyDecColumn) varAlreadyDecTypes) -> 
        updateTree (addErrorsNode node [ErrorChecker (varAlreadyDecLine,varAlreadyDecColumn) $ ErrorVarAlreadyDeclared (l,c) identifier]) tree
      Nothing -> updateTree (addEntryNode identifier (Variable Normal (l,c) types) node) tree

typeCheckerDeclExpression environment decExp = case decExp of
  ExprDecArray arrays@(ArrayInit _ expression@(exp:exps) _) -> foldl (typeCheckerDeclArrayExp environment) (DataChecker (Array Infered (Fix 0, Fix 0)) []) expression
  ExprDec exp -> typeCheckerExpression environment exp

typeCheckerDeclArrayExp environment types expression = case (typeCheckerDeclExpression environment expression, types) of
  (DataChecker (Error ty1) er1, DataChecker (Error ty2 ) er2 ) -> DataChecker (Error ty1) (er1 ++ er2)
  (err@(DataChecker (Error _ ) _), _ ) -> err
  ( _, err@(DataChecker (Error _ ) _)) -> err
  (DataChecker typesFound errorsExp, DataChecker (Array typesInfered (first, Fix n)) errorsTy) -> case supdecl "array" (getExprDeclPos expression) typesInfered typesFound of
    DataChecker (Error ty) e -> DataChecker (Error ty) ((modErrorsPos (getExprDeclPos expression) e) ++ errorsExp ++ errorsTy)
    DataChecker typesChecked e -> DataChecker (Array typesChecked (first, Fix $ n + 1)) (e ++ errorsExp ++ errorsTy)

typeCheckerBuildArrayParameter enviroment array = case array of
  ArrayDimSingle leftBound _ _ rightBound -> typeCheckerBuildArrayBound enviroment leftBound rightBound
  ArrayDimBound elements -> typeCheckerBuildArrayBound enviroment (ArratBoundConst $ Eint $ PInteger ((-1,-1), "0") ) elements 

typeCheckerBuildArrayBound enviroment lBound rBound = 
  let DataChecker leftBound leftErrors = typeCheckerBuildArrayBound' lBound; DataChecker rightBound rightErrors = typeCheckerBuildArrayBound' rBound in DataChecker (leftBound, rightBound) (leftErrors ++ rightErrors)
  where
    typeCheckerBuildArrayBound' bound = case bound of 
      ArrayBoundIdent id@(PIdent (loc,name)) -> case getVarType id enviroment of 
       DataChecker (Error _ ) err -> DataChecker (Fix $ -1) (modErrorsPos loc err)
       DataChecker Int _-> DataChecker (Var name) []
       DataChecker types _-> DataChecker (Fix $ -1) [ErrorChecker loc $ ErrorDeclarationBoundNotCorrectType types name]
      ArratBoundConst constant -> case constant of
        Efloat (PDouble (loc, id)) -> DataChecker (Fix $ -1) [ErrorChecker loc $ ErrorDeclarationBoundArray Real id]
        Echar (PChar (loc,id)) -> DataChecker (Fix $ -1) [ErrorChecker loc $ ErrorDeclarationBoundArray  Char id]
        Eint (PInteger (loc,dimension)) -> DataChecker (Fix $ read dimension) []

typeCheckerExpression environment exp = case exp of
    EAss e1 (AssgnEq (PAssignmEq ((l,c),_)) ) e2 -> typeCheckerExpression' environment (supdecl "assignment") (l,c) e1 e2
    Eplus e1 (PEplus ((l,c),_)) e2 -> typeCheckerExpression' environment sup (l,c) e1 e2
    Eminus e1 (PEminus ((l,c),_)) e2 -> typeCheckerExpression' environment sup (l,c) e1 e2
    Ediv e1 (PEdiv ((l,c),_)) e2 -> typeCheckerExpression' environment sup (l,c) e1 e2
    Etimes e1 (PEtimes ((l,c),_)) e2 -> typeCheckerExpression' environment sup (l,c) e1 e2
    InnerExp _ e _ -> typeCheckerExpression environment e
    Elthen e1 pElthen e2 -> typeCheckerExpression' environment supBool (getExpPos e1) e1 e2
    Epreop (Indirection _) e1 -> typeCheckerExpression environment e1 -- todo: e' un pointer?
    Epreop (Address _) e1 -> if isExpVar e1
      then let DataChecker ty errors = typeCheckerExpression environment e1 in DataChecker (Pointer ty) errors
      else DataChecker (Error Nothing) [ErrorChecker (-4,-4) ErrorCantAddressAnExpression]
    --Da fare ar declaration
    Earray expIdentifier arDeclaration -> typeCheckerDeclarationArray  environment expIdentifier arDeclaration
    EFun funidentifier _ passedparams _ -> eFunTypeChecker funidentifier passedparams environment
    Evar identifier -> getVarType identifier environment
    Econst (Eint _) -> DataChecker Int []
    Econst (Efloat _) -> DataChecker Real []
-- todo: aggiungere tutti i casi degli operatori esistenti

eFunTypeChecker funidentifier passedparams environment = 
  case getVarType funidentifier environment of
    e@(DataChecker (Error (Just ty)) errors) -> e
    e@(DataChecker (Error Nothing) er) -> e
    _otherwhise -> checkPassedParams funidentifier passedparams (getFunParams funidentifier environment) environment _otherwhise


checkPassedParams funidentifier [] [] environment acc@(DataChecker tye errors) = acc

checkPassedParams funidentifier@(PIdent ((l,c),_)) [] (e:expectedParams) environment acc@(DataChecker tye errors) = 
  (DataChecker tye (errors ++ [ErrorChecker (l,c) ErrorCalledProcWithLessArgs]))

checkPassedParams funidentifier@(PIdent ((l,c),_)) (p:passedParams) [] environment acc@(DataChecker tye errors) = 
  (DataChecker tye (errors ++ [ErrorChecker (l,c) ErrorCalledProcWithTooMuchArgs]))


checkPassedParams funidentifier (p:passedParams) (e:expectedParams) environment acc@(DataChecker tye errors) =
  case checkCorrectTypeOfParam p e environment of
    Nothing -> checkPassedParams funidentifier passedParams expectedParams environment acc
    Just err -> checkPassedParams funidentifier passedParams expectedParams environment (DataChecker tye (errors ++ err))

-- parametro che mi aspetto sia un intero
checkCorrectTypeOfParam (PassedPar exp) (Variable Normal (l1,c1) Int) environment = case
  typeCheckerExpression environment exp of
    (DataChecker Int errors) -> Just errors
    (DataChecker tye errors) -> Just (errors ++ [ErrorChecker (getExpPos exp) (ErrorCalledProcWithWrongTypeParam tye Int)])

-- parametro che mi aspetto sia un real
checkCorrectTypeOfParam (PassedPar exp) (Variable Normal (l1,c1) Real) environment = case
  typeCheckerExpression environment exp of
    (DataChecker Int errors) -> Just errors
    (DataChecker Real errors) -> Just errors
    (DataChecker tye errors) -> Just (errors ++ [ErrorChecker (l1,c1) (ErrorCalledProcWithWrongTypeParam tye Int)])

-- parametro che mi aspetto sia un char
checkCorrectTypeOfParam (PassedPar exp) (Variable Normal (l1,c1) Char) environment = case
  typeCheckerExpression environment exp of
    (DataChecker Char errors) -> Just errors
    (DataChecker tye errors) -> Just (errors ++ [ErrorChecker (l1,c1) (ErrorCalledProcWithWrongTypeParam tye Int)])

-- parametro che mi aspetto sia un char
checkCorrectTypeOfParam (PassedPar exp) (Variable Normal (l1,c1) Bool) environment = case
  typeCheckerExpression environment exp of
    (DataChecker Bool errors) -> Just errors
    (DataChecker tye errors) -> Just (errors ++ [ErrorChecker (l1,c1) (ErrorCalledProcWithWrongTypeParam tye Int)])


typeCheckerExpression' environment supCheck loc e1 e2 =
  let DataChecker tye1 errors1 = typeCheckerExpression environment e1; 
      DataChecker tye2 errors2 = typeCheckerExpression environment e2;
      allErrors = errors1 ++ errors2 in 
    case supCheck loc tye1 tye2 of
      DataChecker (Error (Just ty)) errors -> DataChecker ty (errors ++ allErrors) 
      DataChecker (Error Nothing) errors -> DataChecker (Error Nothing) (errors ++ allErrors) 
      DataChecker types errors-> DataChecker types (errors ++ allErrors)    

typeCheckerDeclarationArray environment exp (ArrayInit _ arrays _ ) = case exp of
  Evar (PIdent ((l,c), identifier)) -> case typeCheckerExpression environment exp of
    DataChecker types@(Array _ _) expErrors -> let DataChecker dimension errors = getDeclarationDimension environment arrays in 
      case (getArrayDimension types, dimension) of
        (arrayDim, callDim) -> if arrayDim >= callDim 
          then let typeFound = getSubarrayDimension types callDim in if null errors
            then DataChecker typeFound []
            else DataChecker typeFound (errors ++ expErrors)
          else DataChecker (Error Nothing) ([ErrorChecker (l,c) $ ErrorWrongDimensionArray arrayDim callDim identifier] ++ errors ++ expErrors)
    DataChecker types expErrors -> DataChecker (Error Nothing ) ([ErrorChecker (getExpPos exp) $ ErrorDeclarationBoundNotCorrectType types identifier] ++ expErrors)
  _ -> DataChecker (Error Nothing) [ ErrorChecker (getExpPos exp) ErrorArrayCallExpression]

getDeclarationDimension environment [] = DataChecker 0 [] 
getDeclarationDimension environment (array:arrays) = let DataChecker dimension errors = getDeclarationDimension environment arrays in 
  case array of
    ExprDec exp -> let DataChecker ty expErrors = typeCheckerExpression environment exp in  case sup (getExpPos exp) Int ty of
      DataChecker (Error _) errorsFound -> DataChecker (dimension + 1) (errors ++ errorsFound ++ expErrors)
      _ -> DataChecker (dimension + 1) (errors ++ expErrors)
    expDecl -> DataChecker (dimension + 1) ([ErrorChecker (getExprDeclPos expDecl) ErrorArrayExpressionRequest] ++ errors)

getSubarrayDimension types 0 = types
getSubarrayDimension (Array subtype _) i = getSubarrayDimension subtype (i - 1)

getArrayDimension (Array subtype _) = 1 + getArrayDimension subtype
getArrayDimension _ = 0  

sup pos@(l,c) (Pointer ty) Real = DataChecker (Error (Just ty)) [ErrorChecker pos ErrorCantAddRealToAddress]
sup pos@(l,c) Real (Pointer ty) = DataChecker (Error (Just ty)) [ErrorChecker pos ErrorCantAddRealToAddress]
sup pos@(l,c) (Pointer ty) Char = DataChecker (Error (Just ty)) [ErrorChecker pos ErrorCantAddCharToAddress]
sup pos@(l,c) Char (Pointer ty) = DataChecker (Error (Just ty)) [ErrorChecker pos ErrorCantAddCharToAddress]

sup pos@(l,c) (Pointer _p) Int = DataChecker (Pointer _p) []
sup pos@(l,c) Int (Pointer _p) = DataChecker (Pointer _p) []

sup c Int Int = DataChecker Int []
sup (l,c) Int Real = DataChecker Real []
sup (l,c) Real Int =  DataChecker Real []
sup (l,c) Real Real = DataChecker Real []
sup (l,c) (Error ty1) (Error ty2 ) = DataChecker (Error ty1) []
sup (l,c) e1@(Error _ ) _ =  DataChecker e1 []
sup (l,c) _ e1@(Error _ ) =  DataChecker e1 []
sup (l,c) (Array typesFirst dimensionFirst) (Array typesSecond _) = 
  let DataChecker types errors = sup (l,c) typesFirst typesSecond in case types of 
    err@(Error _ ) -> DataChecker err errors
    _ -> DataChecker (Array types dimensionFirst) []
sup (l,c) array@(Array _ _) types = DataChecker (Error Nothing) [ErrorChecker (l,c) $ ErrorIncompatibleTypes array types]
sup (l,c) types array@(Array _ _) = DataChecker (Error Nothing) [ErrorChecker (l,c) $ ErrorIncompatibleTypes types array]

-- infer del tipo tra due expr messe in relazione tramite un operatore booleano binario
supBool (l,c) e1@(Error _ ) _ = DataChecker e1 []
supBool (l,c) _ e1@(Error _ ) = DataChecker e1 []
supBool (l,c) e1 e2 = supBool' (l,c) e1 e2

supBool' (l,c) error@(Error _ ) _ = DataChecker error []
supBool' (l,c)  _ error@(Error _ ) = DataChecker error []
supBool' (l,c) Int Int = DataChecker Bool []
supBool' (l,c) Real Real =DataChecker Bool []
supBool' (l,c) _ _ = DataChecker Bool []

supdecl id (l,c) Infered types = DataChecker types []
supdecl id (l,c) types Infered = DataChecker types []
supdecl id (l,c) Int Int = DataChecker Int []
supdecl id (l,c) Int Real = DataChecker (Error Nothing) [ErrorChecker (l,c) $ ErrorIncompatibleDeclTypes id Int Real]
supdecl id (l,c) Real Int = DataChecker Real []
supdecl id (l,c) Real Real = DataChecker Real []
supdecl id (l,c) (Error ty1) (Error ty2) = DataChecker (Error ty1) []
supdecl id (l,c) error@(Error _ ) _ = DataChecker error []
supdecl id (l,c) ty error@(Error _ ) = DataChecker ty []
supdecl id (l,c)  ar1@(Array typesFirst dimensionFirst) ar2@(Array typesSecond _) = 
  let DataChecker types errors = supdecl id (l,c) typesFirst typesSecond;
      errorConverted = map (errorIncopatibleTypesChange ar1 ar2) errors in 
    case types of 
      err@(Error _ ) -> DataChecker err errorConverted
      _ -> DataChecker (Array types dimensionFirst) errorConverted
supdecl id (l,c) array@(Array _ _) types = DataChecker array [ErrorChecker (l,c) $ ErrorIncompatibleDeclTypes id array types]
supdecl id (l,c) types array@(Array _ _) = DataChecker types [ErrorChecker (l,c) $ ErrorIncompatibleDeclTypes id types array]

errorIncopatibleTypesChange ty1 ty2 (ErrorChecker (l,c) (ErrorIncompatibleDeclTypes id array types)) = 
  ErrorChecker (l,c) $ ErrorIncompatibleDeclTypes id ty1 ty2
errorIncopatibleTypesChange ty1 ty2 error = error

convertTypeSpecToTypeInferred Tint {} = Int
convertTypeSpecToTypeInferred Treal {} = Real
--convertTypeSpecToTypeInferred (Type Tchar) = Char

convertMode PRef {} = Ref
