module Checker.TypeChecker where


import Control.Monad.Trans.State
import qualified Data.Map as DMap
import AbsChapel
import Checker.SymbolTable
import Checker.BPTree
import Data.Maybe
import Debug.Trace

--type Env = DMap.Map PIdent EnvEntry



-- initVarError = ErrorInitExpr


--type MonState = State EnvEntry 

startState = (DMap.empty, Checker.BPTree.Node {Checker.BPTree.id = "0", val = BP {symboltable = DMap.empty, statements = [], errors = []}, parentID = Nothing, children = []}, "0")


typeChecker (Progr p) = typeCheckerModule p

typeCheckerModule (Mod m) = typeCheckerExt m

-- funzione per il typeChecking
-- usa la state monad per memorizzare info sui tipi di variabile
-- da fare: al posto dello stato contenente solo la Data.Map var -> tipo
-- implementare stato che e' una coppia del tipo (Data.Map(quella di prima), abstract syntax arricchito con info di tipo)
createId l c name = show l ++ "_" ++ show c ++ "_" ++ name

typeCheckerExt [] = get
-- implementato solo il typechecker per le dichiarazioni globali, manca tutta la parte di fun
-- alcuni dettagli sulle due map:
-- 1) (map convertTypeSpecToTypeInferred types) fa semplicemente una traduzione dai tipi Tint.. messi dal parser in tipi Int.. del typechecker
-- il campo declList e' una lista che contiene gli elem x,y,z=3 o x,y,z:int=5
-- dato che questi sono tutti di tipo int posso iterare facilmente usando una mapM_ (non map perche' ci sono le monadi di mezzo)


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
    _ -> do
      typeCheckerParams params
      (symtable, tree, currentIdNode) <- get
      let node = findNodeById currentIdNode tree; variables = map (snd.snd) (DMap.toAscList symtable)  in
        put (symtable,  updateTree (addEntryNode identifier (Function (line,column) variables types) node) tree, currentIdNode )
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
    modify(\(sym,_tree,_id) -> (DMap.insert identifier (identifier, Variable mode (line,column) typefound) sym, _tree, _id))

  
typeCheckerBody identifier (BodyBlock  _ xs _  ) = do
  -- create new child for the blk and enter in it
  (sym, tree, current_id) <- get
  let actualNode = findNodeById current_id tree; child = createChild identifier actualNode in
     put (DMap.empty, addChild actualNode (setSymbolTable sym child) tree, identifier)
  -- process body
  mapM_ typeCheckerBody' xs
  modify(\(sym,_tree,_id) -> (sym,_tree,current_id))
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
          Error _ -> do 
            (sym, tree, current_id) <- get
            get
          _ -> do
            (sym, tree, current_id) <- get
            get
      else do -- caso di lvalue non var
        (sym, tree, current_id) <- get
        -- put (DMap.insert (getAssignOpPos eqsym) (getAssignOpTok eqsym, Assignm (getAssignOpPos eqsym) Error) sym, tree, current_id )
        get
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
  (_sym, _tree, current_id) <- get
  case typeCheckerExpression (_sym, _tree, current_id) expression of
    Bool -> get -- questo dovrebbe essere l'unico caso accettato, se expression non e' bool devo segnalarlo come errore
    _ -> get


typeCheckerDeclaration x = do
  environment <- get
  case x of
    NoAssgmDec identifiers colon types -> typeCheckerIdentifiers identifiers (convertTypeSpecToTypeInferred types) Infered
    NoAssgmArrayFixDec identifiers colon array -> typeCheckerIdentifiers identifiers (typeCheckerBuildArrayType environment array Infered) Infered
    NoAssgmArrayDec identifiers colon array types -> typeCheckerIdentifiers identifiers (typeCheckerBuildArrayType environment array (convertTypeSpecToTypeInferred types)) Infered
    AssgmTypeDec identifiers colon types assignment exp -> typeCheckerIdentifiers identifiers (convertTypeSpecToTypeInferred types) (typeCheckerDeclExpression environment exp)
    AssgmArrayTypeDec identifiers colon array types assignment exp -> typeCheckerIdentifiers identifiers  (typeCheckerBuildArrayType environment array (convertTypeSpecToTypeInferred types)) (typeCheckerDeclExpression environment exp)
    AssgmArrayDec identifiers colon array assignment exp -> typeCheckerIdentifiers identifiers (typeCheckerBuildArrayType environment array Infered) (typeCheckerDeclExpression environment exp)
    AssgmDec identifiers assigment exp -> typeCheckerIdentifiers identifiers (typeCheckerDeclExpression environment exp) Infered

typeCheckerBuildArrayType environment (ArrayDeclIndex _ array _) = typeCheckerBuildArrayType' environment array
  where
    typeCheckerBuildArrayType' enviroment [] types = types
    typeCheckerBuildArrayType' enviroment (array:arrays) types = Array (typeCheckerBuildArrayType' enviroment arrays types) (typeCheckerBuildArrayParameter enviroment array)

typeCheckerIdentifiers identifiers typeLeft typeRight =
   mapM_ (typeCheckerIdentifier (supdecl (-2,-2) typeLeft typeRight)) identifiers

typeCheckerIdentifier types id@(PIdent ((l,c), _)) = do
  (symtable, tree, current_id) <- get
  case types of
    Error error -> do
      let node = findNodeById current_id tree in
        modify (\(symtable,tree,current_id) -> (symtable, updateTree (addErrorNode (modErrorPos error (l,c)) node) tree , current_id )) 
      get
    _ -> do
      modify (\(symtable,tree,current_id) -> (symtable, typeCheckerVariable id tree types current_id, current_id ))
      get

typeCheckerVariable (PIdent ((l,c), identifier)) tree types currentIdNode = 
  let node = findNodeById currentIdNode tree in case DMap.lookup identifier (getSymbolTable node) of
      Just (varAlreadyDeclName, Variable _ (varAlreadyDecLine,varAlreadyDecColumn) varAlreadyDecTypes) -> 
        updateTree (addErrorNode (ErrorVarAlreadyDeclared (varAlreadyDecLine,varAlreadyDecColumn) (l,c) identifier ) node) tree
      Nothing -> updateTree (addEntryNode identifier (Variable Normal (l,c) types) node) tree

typeCheckerDeclExpression environment decExp = case decExp of
  ExprDecArray arrays@(ArrayInit _ expression@(exp:exps) _) -> foldr (typeCheckerDeclArrayExp environment) (Array Infered (Fix 0, Fix 0)) expression
  ExprDec exp -> typeCheckerExpression environment exp

typeCheckerDeclArrayExp environment expression types = case (typeCheckerDeclExpression environment expression, types) of
  (err@(Error _), _ ) -> err
  ( _, err@(Error _) ) -> err
  (typesFound, Array typesInfered (first, Fix n)) -> case supdecl (-1,-1) typesFound typesInfered of
    err@(Error e) -> Error (modErrorPos e (getExprDeclPos expression))
    typesChecked -> Array typesChecked (first,Fix $ n + 1)

typeCheckerBuildArrayParameter enviroment array = case array of
  ArrayDimSingle leftBound _ _ rightBound -> typeCheckerBuildArrayBound enviroment leftBound rightBound
  ArrayDimBound elements -> typeCheckerBuildArrayBound enviroment (ArratBoundConst $ Eint $ PInteger $ ((-1,-1), "0") ) elements 

typeCheckerBuildArrayBound enviroment leftBound rightBound = (typeCheckerBuildArrayBound' leftBound,  typeCheckerBuildArrayBound' rightBound)
  where
    typeCheckerBuildArrayBound' bound = case bound of 
      ArrayBoundIdent (PIdent ((_,_),identifier)) -> Var identifier
      ArratBoundConst constant -> case constant of
        Efloat _ -> Fix 0
        Echar _ -> Fix  0
        Eint (PInteger ((_,_),dimension)) -> Fix $ read dimension

typeCheckerExpression environment@(_sym, _tree, current_id) exp = case exp of
    EAss e1 (AssgnEq (PAssignmEq ((l,c),_)) ) e2 -> supdecl (getExpPos e1) (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
    Eplus e1 (PEplus ((l,c),_)) e2 -> sup (l,c) (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
    Eminus e1 (PEminus ((l,c),_)) e2 -> sup (l,c) (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
    Ediv e1 (PEdiv ((l,c),_)) e2 -> sup (l,c) (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
    Etimes e1 (PEtimes ((l,c),_)) e2 -> sup (l,c) (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
    InnerExp _ e _ -> typeCheckerExpression environment e
    Elthen e1 pElthen e2 -> supBool (typeCheckerExpression environment e1) (typeCheckerExpression environment e2) 
    --Da fare ar declaration
    Earray expIdentifier arDeclaration -> typeCheckerExpression environment expIdentifier
    Evar identifier -> getVarType identifier environment
    Econst (Eint _) -> Int
    Econst (Efloat _) -> Real
  -- todo: aggiungere tutti i casi degli operatori esistenti


-- infer del tipo fra due tipi. Da capire meglio cosa fare in caso di errore.

sup = sup'

sup' (l,c) Int Int = Int
sup' (l,c) Int Real = Real
sup' (l,c) Real Int =  Real
sup' (l,c) Real Real =  Real
sup' (l,c) e1@(Error _) _ =  e1
sup' (l,c) _ e1@(Error _) =  e1
sup' (l,c) (Array typesFirst dimensionFirst) (Array typesSecond _) = let types = sup' (l,c) typesFirst typesSecond in Array types dimensionFirst
sup' (l,c) array@(Array _ _) types =  Error (ErrorIncompatibleTypes (l,c) array types)
sup' (l,c) types array@(Array _ _) =  Error (ErrorIncompatibleTypes (l,c) types array)

-- infer del tipo tra due expr messe in relazione tramite un operatore booleano binario
supBool e1@(Error _) _ = e1
supBool _ e1@(Error _) = e1
supBool e1 e2 = supBool' e1 e2


supBool' error@(Error _) _ = error
supBool' _ error@(Error _) = error
supBool' Int Int = Bool
supBool' Real Real = Bool
supBool' _ _ = Bool

-- infer del tipo nel caso di dichiarazioni con inizializzazione della variabile
-- e' leggermente diversa dalla sup definita sopra, sotto un commento al riguardo.
-- supdecl Int Int = Int
-- errore perche' questo corrisponde a codice del tipo: int x = 1.3; che deve ovviamente dare errore di tipo

supdecl = supdecl'

supdecl' (l,c) Infered types = types
supdecl' (l,c) types Infered = types
supdecl' (l,c) Int Int = Int
supdecl' (l,c) Int Real = Error (ErrorIncompatibleTypes (l,c) Int Real)
supdecl' (l,c) Real Int = Real
supdecl' (l,c) Real Real = Real
supdecl' (l,c) error@(Error _ ) _ = error
supdecl' (l,c) _ error@(Error _) = error
supdecl' (l,c) (Array typesFirst dimensionFirst) (Array typesSecond _) = let types = supdecl' (l,c) typesFirst typesSecond in Array types dimensionFirst
supdecl' (l,c) array@(Array _ _) types =  Error (ErrorIncompatibleTypes (l,c) array types)
supdecl' (l,c) types array@(Array _ _) =  Error (ErrorIncompatibleTypes (l,c) types array)


convertTypeSpecToTypeInferred Tint {} = Int
convertTypeSpecToTypeInferred Treal {} = Real
--convertTypeSpecToTypeInferred (Type Tchar) = Char

convertMode PRef {} = Ref
