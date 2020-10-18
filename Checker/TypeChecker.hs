module Checker.TypeChecker where


import Control.Monad.Trans.State
import qualified Data.Map as DMap
import AbsChapel
import Checker.SymbolTable
import Checker.BPTree

--type Env = DMap.Map PIdent EnvEntry



initVarError = ErrorInitExpr


--type MonState = State EnvEntry 

startState = (0, (DMap.insert 0 DMap.empty DMap.empty), DMap.empty, Checker.BPTree.Node {Checker.BPTree.id = "0", val = "tregt", parentID = Nothing, children = []}, "0")


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
    ExtFun (FunDec (PProc ((l,c),funname)  ) signature body) -> do
        typeCheckerSignature signature
        typeCheckerBody (createId l c funname) body
        typeCheckerExt xs



typeCheckerSignature signature = case signature of
  SignNoRet identifier (FunParams _ params _) -> get
  SignWRet identifier (FunParams _ params _) _ types -> get

typeIncreaseLevel _ = do
  (depth, env, _sym, _tree, _current_id) <- get
  put (depth + 1, env, _sym, _tree, _current_id)

typeDecreaseLevel _ = do
  (depth, env, _sym, _tree, _current_id) <- get
  put (depth - 1, env, _sym, _tree, _current_id)  

typeCheckerBody identifier (FunBlock  _ xs _  ) = do

  (depth, env, _sym, tree, current_id) <- get
  let actualNode = findNodeById current_id tree in
    put (depth, env, _sym, (addChild actualNode (createChild identifier tree) tree), identifier)
  typeIncreaseLevel xs

  mapM_ typeCheckerBody' xs

  typeDecreaseLevel xs
  (depth, env, _sym, tree, _) <- get
  put (depth, env, _sym, tree, current_id)

  get

typeCheckerBody' x = do
  case x of
    Stm statement -> typeCheckerStatement statement
    Fun _ _ -> get
    DeclStm (Decl decMode declList _ ) -> do
      mapM_ typeCheckerDeclaration declList
      get
    Block body -> typeCheckerBody "a" body
  get

typeCheckerStatement statement = case statement of
  DoWhile _do _while body guard -> do
    typeCheckerBody "a" body
    typeCheckerGuard guard
    get
  While _while guard body -> do
    typeCheckerGuard guard
    typeCheckerBody "a" body
    get
  If _if guard _then body -> do
    typeCheckerGuard guard
    typeCheckerBody "a" body
    get
  IfElse _if guard _then bodyIf _else bodyElse -> do
    typeCheckerGuard guard
    typeCheckerBody "a" bodyIf
    typeCheckerBody "a" bodyElse
    get
  StExp exp@(EAss e1 eqsym e2) _semicolon -> do
    case (isExpVar e1) of
      True -> do
        env <- get
        case (typeCheckerExpression env (EAss e1 eqsym e2)) of
          Error -> do 
            (d,e,sym, tree, current_id) <- get
            put (d,e, DMap.insert (getVarPos e1) (getVarId e1, (Variable (getVarPos e1) Error)) sym, tree, current_id )
            get
          otherwhise -> do
            (d,e,sym, tree, current_id) <- get
            put (d,e, DMap.insert (getVarPos e1) (getVarId e1, (Variable (getVarPos e1) otherwhise)) sym, tree, current_id )
            get
      False -> do -- caso di lvalue non var
        (d,e,sym, tree, current_id) <- get
        put (d,e, DMap.insert (getAssignOpPos eqsym) (getAssignOpTok eqsym, (Assignm (getAssignOpPos eqsym) Error)) sym, tree, current_id )
        get
  RetVal _ _ _ -> get
  RetVoid _ _ -> get

      

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
  (depth, env, _sym, _tree, current_id) <- get
  case (typeCheckerExpression (depth, env,_sym, _tree, current_id) expression) of
    Bool -> get -- questo dovrebbe essere l'unico caso accettato, se expression non e' bool devo segnalarlo come errore
    _ -> get


typeCheckerDeclaration x = case x of
    NoAssgmDec identifiers colon types -> typeCheckerIdentifiers identifiers types
    NoAssgmArrayFixDec identifiers colon array -> typeCheckerIdentifiersArray identifiers array Nothing
    NoAssgmArrayDec identifiers colon array types -> typeCheckerIdentifiersArray identifiers array (Just types)
    AssgmTypeDec identifiers colon types assignment exp -> typeCheckerIdentifiersWithExpression identifiers (Just types) exp
    AssgmArrayTypeDec identifiers colon array types assignment exp -> typeCheckerIdentifiersArrayWithExpression identifiers array (Just types) exp
    AssgmArrayDec identifiers colon array assignment exp -> typeCheckerIdentifiersArrayWithExpression identifiers array Nothing exp
    AssgmDec identifiers assigment exp -> typeCheckerIdentifiersWithExpression identifiers Nothing exp
    

typeCheckerIdentifiersArray identifiers array types = 
   mapM_ (typeCheckerIdentifier (typeCheckerArray array types)) identifiers    

typeCheckerIdentifiersArrayWithExpression identifiers array types exp = do
  environment <- get
  case types of
    Nothing -> mapM_ (typeCheckerIdentifier (typeCheckerExpression environment exp)) identifiers
    Just defineType -> mapM_ (typeCheckerIdentifier (supdecl (typeCheckerArray array types) (typeCheckerExpression environment exp))) identifiers 


typeCheckerArray array types = case types of
  Nothing -> Int
  Just definedType -> Int


typeCheckerIdentifiers identifiers types = 
   mapM_ (typeCheckerIdentifier (convertTypeSpecToTypeInferred types)) identifiers
    
typeCheckerIdentifiersWithExpression identifiers types exp = do
  environment <- get
  case types of
    Nothing -> mapM_ (typeCheckerIdentifier (typeCheckerExpression environment exp)) identifiers
    Just defineType -> mapM_ (typeCheckerIdentifier (supdecl (convertTypeSpecToTypeInferred defineType) (typeCheckerExpression environment exp))) identifiers

-- typechecking nel caso di dichiarazioni senza inizializzazione
typeCheckerIdentifier types (PIdent ((line,column), identifier)) = do
  (depth, env, symtable, tree, current_id) <- get
  put (depth, DMap.insert depth (DMap.insert identifier (Variable (line,column) types) (getDepthMap (depth, env, symtable, tree))) env, DMap.insert (line,column) (identifier, (Variable (line,column) types)) symtable, tree, current_id )
  get

typeCheckerExpression environment@(depth, env, _sym, _tree, current_id) expression = case expression of
  Eplus e1 plus e2 -> sup (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
  Eminus e1 minus e2 -> sup (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
  Ediv e1 div e2 -> sup (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
  Etimes e1 div e2 -> sup (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
  InnerExp _ e _ -> typeCheckerExpression environment e
  Elthen e1 pElthen e2 -> supBool (typeCheckerExpression environment e1) (typeCheckerExpression environment e2) 
  Evar (PIdent (_, identifier)) -> getVarType identifier environment
  EAss e1 _eqsym e2 -> supdecl (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
  Econst (Eint _) -> Int
  Econst (Efloat _) -> Real
  -- todo: aggiungere tutti i casi degli operatori esistenti



-- typechecking nel caso di dichiarazioni con inizializzazione
--typeCheckerGlobalDecls'' (t:types) (InitDecl (NoPointer (Name (Ident ((l,c),name)))) (InitExpr expr) ) = do
--    env <-get
--    put (DMap.insert name (Variable (l,c) ((supdecl t (typeOf env expr)):[]) ) env)
--    get
getDepthMap (depth,env, _sym, _tree) = case DMap.lookup depth env of
  Just envDepth -> envDepth
  Nothing -> DMap.empty

getVarType var (0,env, _sym, _tree, current_id) = case DMap.lookup 0 env of
  Just variables -> case DMap.lookup var variables of
    Just (Variable _ t) -> t
    Nothing -> Error
  Nothing -> Error
getVarType var (depth,env, _sym, _tree, _current_id) = case DMap.lookup depth env of
  Just variables -> case DMap.lookup var variables of
    Just (Variable _ t) -> t
    Nothing -> case depth of
      0 -> Error
      _ -> getVarType var (depth - 1, env, _sym, _tree, _current_id)
  _ -> getVarType var (depth - 1, env, _sym, _tree, _current_id)

-- infer del tipo fra due tipi. Da capire meglio cosa fare in caso di errore.
sup Int Int = Int
sup Int Real = Real
sup Real Int = Real
sup Real Real = Real
sup Error _ = Error
sup _ Error = Error

-- infer del tipo tra due expr messe in relazione tramite un operatore booleano binario
supBool Error _ = Error
supBool _ Error = Error
supBool Int Int = Bool
supBool Real Real = Bool
supBool _ _ = Bool

-- infer del tipo nel caso di dichiarazioni con inizializzazione della variabile
-- e' leggermente diversa dalla sup definita sopra, sotto un commento al riguardo.
--supdecl Int Int = Int
-- errore perche' questo corrisponde a codice del tipo: int x = 1.3; che deve ovviamente dare errore di tipo
supdecl Int Int = Int
supdecl Int Real = Error
supdecl Real Int = Real
supdecl Real Real = Real
supdecl Error _ = Error
supdecl _ Error = Error


convertTypeSpecToTypeInferred (Tint _) = Int
convertTypeSpecToTypeInferred (Treal _) = Real
--convertTypeSpecToTypeInferred (Type Tchar) = Char
