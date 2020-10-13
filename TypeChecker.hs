module TypeChecker where


import Control.Monad.Trans.State
import qualified Data.Map as DMap
import AbsChapel


--type Env = DMap.Map PIdent EnvEntry

data EnvEntry = 
  Variable Loc TypeChecker.Type 
--  | Function Loc [Parameter] Type 
--  | Constant Literal
 deriving (Show)

data Type = Int | Real | Bool | Error
  deriving (Show)

data Type' = Int' | Real' | Error' ErrorType
  deriving (Show)

data ErrorType = ErrorInitExpr PIdent Type' Type' | ErrorNotInitVar 
  deriving(Show) 

initVarError = ErrorInitExpr

type Loc = (Int,Int)

--type MonState = State EnvEntry 

startState = (0, DMap.insert 0 DMap.empty DMap.empty) 


typeChecker (Progr p) = typeCheckerModule p

typeCheckerModule (Mod m) = typeCheckerExt m

-- funzione per il typeChecking
-- usa la state monad per memorizzare info sui tipi di variabile
-- da fare: al posto dello stato contenente solo la Data.Map var -> tipo
-- implementare stato che e' una coppia del tipo (Data.Map(quella di prima), abstract syntax arricchito con info di tipo)
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
    ExtFun (FunDec _ signature (FunBlock _ statements _ )) -> do
        typeCheckerSignature signature
        --typeIncreaseLevel statements
        typeCheckerBody statements
        --typeDecreaseLevel statements
        typeCheckerExt xs

typeCheckerSignature signature = case signature of
  SignNoRet identifier (FunParams _ params _) -> get
  SignWRet identifier (FunParams _ params _) _ types -> get

typeIncreaseLevel _ = do
  (depth, env) <- get
  put (depth + 1, env)

typeDecreaseLevel _ = do
  (depth, env) <- get
  put (depth - 1, env)  

typeCheckerBody [] = get
typeCheckerBody (x:xs) = do
  typeCheckerBody x
  typeCheckerBody xs
  get


typeCheckerBody' x = do
  typeIncreaseLevel x
  case x of
    Stm statement -> typeCheckerStatement statement
    Fun _ _ -> get
    DeclStm (Decl decMode declList _ ) -> do
      mapM_ typeCheckerDeclaration declList
      get
    Block (FunBlock _ statements _ ) -> typeCheckerBody statements
    RetVal _ _ _ -> get
    RetVoid _ _ -> get
  typeDecreaseLevel x
  get

typeCheckerStatement statement = case statement of
  DoWhile _do _while (FunBlock _ bodyStatements _) guard -> do
    mapM_ typeCheckerBody' bodyStatements
    typeCheckerGuard guard
    get
  While _while guard (FunBlock _ bodyStatements _) -> do
    typeCheckerGuard guard
    mapM_ typeCheckerBody' bodyStatements
    get
  If _if guard _then (FunBlock _ bodyStatements _) -> do
    typeCheckerGuard guard
    mapM_ typeCheckerBody' bodyStatements
    get
  IfElse _if guard _then (FunBlock _ bodyIfStatements _) _else (FunBlock _ bodyElseStatements _) -> do
    typeCheckerGuard guard
    mapM_ typeCheckerBody' bodyIfStatements
    mapM_ typeCheckerBody' bodyElseStatements
    get
  StExp exp _semicolon -> get

typeCheckerGuard (SGuard _ expression _) = do
  (depth, env) <- get
  case (typeCheckerExpression (depth, env) expression) of
    Bool -> get -- questo dovrebbe essere l'unico caso accettato, se expression non e' bool devo segnalarlo come errore
    Int -> get
    _ -> get


typeCheckerDeclaration x = case x of
    NoAssgmDec identifiers colon types -> typeCheckerIdentifiers identifiers types
    AssgmDec identifiers assigment exp -> typeCheckerIdentifiersWithExpression identifiers Nothing exp
    AssgmTypeDec identifiers assignment types colon exp -> typeCheckerIdentifiersWithExpression identifiers (Just types) exp

typeCheckerIdentifiers identifiers types = 
   mapM_ (typeCheckerIdentifier (convertTypeSpecToTypeInferred types)) identifiers
    
typeCheckerIdentifiersWithExpression identifiers types exp = do
  environment <- get
  case types of
    Nothing -> mapM_ (typeCheckerIdentifier (typeCheckerExpression environment exp)) identifiers
    Just defineType -> mapM_ (typeCheckerIdentifier (supdecl (convertTypeSpecToTypeInferred defineType) (typeCheckerExpression environment exp))) identifiers

-- typechecking nel caso di dichiarazioni senza inizializzazione
typeCheckerIdentifier types (PIdent ((line,column), identifier)) = do
  (depth, env) <- get
  put (depth, DMap.insert depth (DMap.insert identifier (Variable (line,column) types) (getDepthMap (depth,env))) env)
  get

typeCheckerExpression environment@(depth, env) expression = case expression of
  Eplus e1 plus e2 -> sup (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
  Elthen e1 pElthen e2 -> supBool (typeCheckerExpression environment e1) (typeCheckerExpression environment e2) 
  Evar (PIdent (_, identifier)) -> getVarType identifier environment
  Econst (Eint _) -> Int
  Econst (Efloat _) -> Real
  -- todo: aggiungere tutti i casi degli operatori esistenti



-- typechecking nel caso di dichiarazioni con inizializzazione
--typeCheckerGlobalDecls'' (t:types) (InitDecl (NoPointer (Name (Ident ((l,c),name)))) (InitExpr expr) ) = do
--    env <-get
--    put (DMap.insert name (Variable (l,c) ((supdecl t (typeOf env expr)):[]) ) env)
--    get
getDepthMap (depth,env) = case DMap.lookup depth env of
  Just envDepth -> envDepth
  Nothing -> DMap.empty

getVarType var (0,env) = case DMap.lookup 0 env of
  Just variables -> case DMap.lookup var variables of
    Just (Variable _ t) -> t
    Nothing -> Error
  Nothing -> Error
getVarType var (depth,env) = case DMap.lookup depth env of
  Just variables -> case DMap.lookup var variables of
    Just (Variable _ t) -> t
    Nothing -> case depth of
      0 -> Error
      _ -> getVarType var (depth - 1, env)
  _ -> getVarType var (depth - 1, env)

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
supdecl Int Real = Error
supdecl Real Int = Real
supdecl Real Real = Real
supdecl Error _ = Error
supdecl _ Error = Error


convertTypeSpecToTypeInferred (Tint _) = Int
convertTypeSpecToTypeInferred (Treal _) = Real
--convertTypeSpecToTypeInferred (Type Tchar) = Char
