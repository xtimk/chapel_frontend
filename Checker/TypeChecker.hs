module Checker.TypeChecker where

import Control.Monad.Trans.State
import qualified Data.Map as DMap
import AbsChapel
import Checker.SymbolTable
import Checker.BPTree
import Data.Maybe
import Debug.Trace

startState = (DMap.empty, [], Checker.BPTree.Node {Checker.BPTree.id = "0", val = BP {symboltable = DMap.empty, statements = [], errors = []}, parentID = Nothing, children = []}, "0")

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
        typeCheckerBody (createId l c funname) body
        typeCheckerExt xs

typeCheckerSignature signature = case signature of
  SignNoRet identifier (FunParams _ params _) -> typeCheckerSignature' identifier params Infered
  SignWRet identifier (FunParams _ params _) _ types -> typeCheckerSignature' identifier params (convertTypeSpecToTypeInferred types)

typeCheckerSignature' (PIdent ((line,column),identifier)) params types = 
  case types of
    Error _ -> get
    (ModeType _ typesFound) -> do
      typeCheckerParams params
      (symtable, _e, tree, currentIdNode) <- get
      let node = findNodeById currentIdNode tree; variables = map (snd.snd) (DMap.toAscList symtable)  in
        put (symtable, _e, updateTree (addEntryNode identifier (Function (line,column) variables typesFound) node) tree, currentIdNode )
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
  let (ModeType _ typefound) = convertTypeSpecToTypeInferred types in 
    modify(\(sym,_e,_t,_i) -> (DMap.insert identifier (identifier, Variable mode (line,column) typefound) sym,_e,_t,_i))

  
typeCheckerBody identifier (BodyBlock  _ xs _  ) = do
  -- create new child for the blk and enter in it
  (sym, _e, tree, current_id) <- get
  let actualNode = findNodeById current_id tree; child = createChild identifier actualNode in
     put (DMap.empty, _e, addChild actualNode (setSymbolTable sym child) tree, identifier)
  -- process body
  mapM_ typeCheckerBody' xs
  modify(\(_s,_e,_t,_) -> (_s,_e,_t,current_id))
  get

typeCheckerBody' x = do
  case x of
    Stm statement -> typeCheckerStatement statement
    Fun _ _ -> get
    DeclStm (Decl decMode declList _ ) -> do
      mapM_ typeCheckerDeclaration declList
      get
    Block body@(BodyBlock (POpenGraph ((l,c), name)) _ _) -> typeCheckerBody (createId l c name) body
  get

typeCheckerStatement statement = case statement of
  DoWhile (Pdo ((l,c),name)) _while body guard -> do
    typeCheckerBody (createId l c name) body
    typeCheckerGuard guard
    get
  While (PWhile ((l,c), name)) guard body -> do
    typeCheckerGuard guard
    typeCheckerBody (createId l c name) body
    get
  If (PIf ((l,c), name)) guard _then body -> do
    typeCheckerGuard guard
    typeCheckerBody (createId l c name) body
    get
  IfElse (PIf ((lIf,cIf), nameIf)) guard _then bodyIf (PElse ((lElse,cElse), nameElse)) bodyElse -> do
    typeCheckerGuard guard
    typeCheckerBody (createId lIf cIf nameIf) bodyIf
    typeCheckerBody (createId lElse cElse nameElse) bodyElse
    get
  StExp exp@(EAss e1 eqsym e2) _semicolon ->
    if isExpVar e1
      then do
        env <- get
        case typeCheckerExpression env (EAss e1 eqsym e2) of
          Error error -> do
            (_s,_e,tree,current_id) <- get
            let node = findNodeById current_id tree in
              modify (\(_s, _e, tree,_i) -> (_s, _e, (updateTree (addErrorsNode node error) tree) , _i ))
            get
          _ ->get -- se non ho errore non faccio nulla nello stato
      else get

  StExp _ _-> get
  RetVal {} -> get
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
    (ModeType Normal Bool) -> get -- questo dovrebbe essere l'unico caso accettato, se expression non e' bool devo segnalarlo come errore
    _otherwhise -> do
      let node = findNodeById current_id tree in
        modify (\(_s, _e, tree,_i) -> (_s, _e, (updateTree (addErrorNode (ErrorGuardNotBoolean (getExpPos expression)) node) tree) , _i ))
      get
      

typeCheckerDeclaration x = do
  environment <- get
  case x of
    NoAssgmDec ids _colon types -> typeCheckerIdentifiers ids (convertTypeSpecToTypeInferred types) Infered
    AssgmDec ids assigment exp -> typeCheckerIdentifiers ids (typeCheckerDeclExpression environment exp) Infered
    AssgmTypeDec ids _colon types assignment exp -> typeCheckerIdentifiers ids (convertTypeSpecToTypeInferred types) (typeCheckerDeclExpression environment exp)
    NoAssgmArrayFixDec ids _colon array -> typeCheckerIdentifiersArray ids array Infered Infered
    NoAssgmArrayDec ids _colon array types -> typeCheckerIdentifiersArray ids array (convertTypeSpecToTypeInferred types) Infered
    AssgmArrayTypeDec ids _colon array types assignment exp -> typeCheckerIdentifiersArray ids array (convertTypeSpecToTypeInferred types) (typeCheckerDeclExpression environment exp)
    AssgmArrayDec ids _colon array assignment exp -> typeCheckerIdentifiersArray ids array Infered (typeCheckerDeclExpression environment exp)

typeCheckerIdentifiersArray ids array typeLeft typeRight = do
  environment <- get
  let (typeArray, errors) = typeCheckerBuildArrayType environment array typeLeft in do
    modify (addErrorsCurrentNode errors)
    typeCheckerIdentifiers ids typeArray typeRight
  get

typeCheckerBuildArrayType environment (ArrayDeclIndex _ array _) = typeCheckerBuildArrayType' environment array
  where
    typeCheckerBuildArrayType' enviroment [] types = (types, [])
    typeCheckerBuildArrayType' enviroment (array:arrays) types = 
      let (bounds, errors) = typeCheckerBuildArrayParameter enviroment array; (typesFound, othersErrors) = typeCheckerBuildArrayType' enviroment arrays types in (ModeType Normal (Array typesFound bounds), errors ++ othersErrors )

typeCheckerIdentifiers identifiers typeLeft typeRight = do
   mapM_ (typeCheckerIdentifier (supModeDecl (-2,-2) typeLeft typeRight)) identifiers
   get

typeCheckerIdentifier types id@(PIdent ((l,c), _)) = do
  (_s,_e,tree,current_id) <- get
  case types of
    Error error -> do
      let node = findNodeById current_id tree in
        modify (\(_s, _e, tree,_i) -> (_s, _e, updateTree (addErrorsNode node (modErrorsPos (l,c) error )) tree , _i )) 
      get
    _ -> do
      modify (\(_s,_e,tree,_i) -> (_s, _e, typeCheckerVariable id tree types current_id, _i ))
      get

typeCheckerVariable (PIdent ((l,c), identifier)) tree (ModeType mode types) currentIdNode = 
  let node = findNodeById currentIdNode tree in case DMap.lookup identifier (getSymbolTable node) of
      Just (varAlreadyDeclName, Variable _ (varAlreadyDecLine,varAlreadyDecColumn) varAlreadyDecTypes) -> 
        updateTree (addErrorsNode node [ErrorVarAlreadyDeclared (varAlreadyDecLine,varAlreadyDecColumn) (l,c) identifier]) tree
      Nothing -> updateTree (addEntryNode identifier (Variable mode (l,c) types) node) tree

typeCheckerDeclExpression environment decExp = case decExp of
  ExprDecArray arrays@(ArrayInit _ expression@(exp:exps) _) -> foldr (typeCheckerDeclArrayExp environment) (ModeType Normal (Array Infered (Fix 0, Fix 0))) expression
  ExprDec exp -> typeCheckerExpression environment exp

typeCheckerDeclArrayExp environment expression types = case (typeCheckerDeclExpression environment expression, types) of
  ((Error er1), (Error er2)) -> Error $ er1 ++ er2
  (err@(Error _), _ ) -> err
  ( _, err@(Error _) ) -> err
  (typesFound, ModeType Normal (Array typesInfered (first, Fix n))) -> case supModeDecl (-1,-1) typesFound typesInfered of
    err@(Error e) -> Error (modErrorsPos (getExprDeclPos expression) e )
    typesChecked ->  ModeType Normal (Array typesChecked (first, Fix $ n + 1))

typeCheckerBuildArrayParameter enviroment array = case array of
  ArrayDimSingle leftBound _ _ rightBound -> typeCheckerBuildArrayBound enviroment leftBound rightBound
  ArrayDimBound elements -> typeCheckerBuildArrayBound enviroment (ArratBoundConst $ Eint $ PInteger ((-1,-1), "0") ) elements 

typeCheckerBuildArrayBound enviroment lBound rBound = 
  let (leftBound, leftErrors) = typeCheckerBuildArrayBound' lBound; (rightBound, rightErrors) = typeCheckerBuildArrayBound' rBound in ((leftBound, rightBound), leftErrors ++ rightErrors)
  where
    typeCheckerBuildArrayBound' bound = case bound of 
      ArrayBoundIdent id@(PIdent (loc,name)) -> case getVarType id enviroment of 
       Error err -> (Fix $ -1, modErrorsPos loc err)
       ModeType Normal Int -> (Var name, [])
       types -> (Fix $ -1, [ErrorDeclarationBoundNotCorrectType loc types name])
      ArratBoundConst constant -> case constant of
        Efloat (PDouble (loc, id)) -> (Fix $ -1, [ErrorDeclarationBoundArray loc (ModeType Normal Real) id])
        Echar (PChar (loc,id)) -> ( Fix $ -1, [ErrorDeclarationBoundArray loc (ModeType Normal Char) id])
        Eint (PInteger (loc,dimension)) -> (Fix $ read dimension, [])

typeCheckerExpression environment exp = case exp of
    EAss e1 (AssgnEq (PAssignmEq ((l,c),_)) ) e2 -> supModeDecl (getExpPos e1) (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
    Eplus e1 (PEplus ((l,c),_)) e2 -> supMode (l,c) (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
    Eminus e1 (PEminus ((l,c),_)) e2 ->  supMode (l,c) (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
    Ediv e1 (PEdiv ((l,c),_)) e2 -> supMode (l,c) (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
    Etimes e1 (PEtimes ((l,c),_)) e2 -> (supMode (l,c) (typeCheckerExpression environment e1) (typeCheckerExpression environment e2))
    InnerExp _ e _ -> (typeCheckerExpression environment e)
    Elthen e1 pElthen e2 -> supModeBool (getExpPos e1) (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
    Epreop (Indirection _) e1 -> (typeCheckerExpression environment e1)
    Epreop (Address _) e1 -> (typeCheckerExpression environment e1)
    --Da fare ar declaration
    Earray expIdentifier arDeclaration -> (typeCheckerExpression environment expIdentifier)
    Evar identifier -> getVarType identifier environment
    Econst (Eint _) -> ModeType Normal Int
    Econst (Efloat _) -> ModeType Normal Real
-- todo: aggiungere tutti i casi degli operatori esistenti


supMode (l,c) (ModeType Ref _) (ModeType Normal Int) = ModeType Ref Int
supMode (l,c) (ModeType Ref _) (ModeType Normal Real) = ModeType Ref Real -- errore
supMode (l,c) (ModeType Normal t1) (ModeType Normal t2) = ModeType Normal (sup (l,c) t1 t2)

-- todo check
supModeDecl (l,c) Infered types = types
supModeDecl (l,c) types Infered = types
supModeDecl (l,c) (ModeType Ref _) (ModeType Normal Int) = ModeType Ref Int
supModeDecl (l,c) (ModeType Ref _) (ModeType Normal Real) = ModeType Ref Real -- errore
supModeDecl (l,c) (ModeType Normal t1) (ModeType Normal t2) = ModeType Normal (supdecl (l,c) t1 t2)

-- todo check
supModeBool (l,c) (ModeType Ref _) (ModeType Normal Int) = ModeType Ref Int
supModeBool (l,c) (ModeType Ref _) (ModeType Normal Real) = ModeType Ref Real -- errore
supModeBool (l,c) (ModeType Normal t1) (ModeType Normal t2) = ModeType Normal (supBool (l,c) t1 t2)


sup (l,c) Int Int = Int
sup (l,c) Int Real = Real
sup (l,c) Real Int =  Real
sup (l,c) Real Real =  Real
sup (l,c) (TypeError er1 ) (TypeError er2 ) = TypeError $ er1 ++ er2
sup (l,c) e1@(TypeError _) _ =  e1
sup (l,c) _ e1@(TypeError _) =  e1
sup (l,c) (Array typesFirst dimensionFirst) (Array typesSecond _) = 
  let types = supModeDecl (l,c) typesFirst typesSecond in case types of 
    err@(Error error) -> TypeError error
    _ -> Array types dimensionFirst
sup (l,c) array@(Array _ _) types =  TypeError [ErrorIncompatibleTypes (l,c) array types]
sup (l,c) types array@(Array _ _) =  TypeError [ErrorIncompatibleTypes (l,c) types array]

-- infer del tipo tra due expr messe in relazione tramite un operatore booleano binario
supBool (l,c) e1@(TypeError _) _ = e1
supBool (l,c) _ e1@(TypeError _) = e1
supBool (l,c) e1 e2 = supBool' e1 e2

supBool' error@(TypeError _) _ = error
supBool' _ error@(TypeError _) = error
supBool' Int Int = Bool
supBool' Real Real = Bool
supBool' _ _ = Bool

--supdecl (l,c) Infered types = types
--supdecl (l,c) types Infered = types
supdecl (l,c) Int Int = Int
supdecl (l,c) Int Real = TypeError [ErrorIncompatibleTypes (l,c) Int Real]
supdecl (l,c) Real Int = Real
supdecl (l,c) Real Real = Real
supdecl (l,c) (TypeError er1 ) (TypeError er2 ) = TypeError $ er1 ++ er2
supdecl (l,c) error@(TypeError _ ) _ = error
supdecl (l,c) _ error@(TypeError _) = error
supdecl (l,c) (Array typesFirst dimensionFirst) (Array typesSecond _) = 
  let types = supModeDecl (l,c) typesFirst typesSecond in case types of 
    err@(Error error) -> TypeError error
    _ -> Array types dimensionFirst
supdecl (l,c) array@(Array _ _) types =  TypeError [ErrorIncompatibleTypes (l,c) array types]
supdecl (l,c) types array@(Array _ _) =  TypeError [ErrorIncompatibleTypes (l,c) types array]

convertTypeSpecToTypeInferred Tint {} = ModeType Normal Int
convertTypeSpecToTypeInferred Treal {} = ModeType Normal Real
--convertTypeSpecToTypeInferred (Type Tchar) = Char

convertMode PRef {} = Ref
