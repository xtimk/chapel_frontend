module TypeChecker where


import Control.Monad.Trans.State
import qualified Data.Map as DMap
import AbsChapel


type Env = DMap.Map PIdent EnvEntry

data EnvEntry = 
  Variable Loc TypeChecker.Type 
--  | Function Loc [Parameter] Type 
--  | Constant Literal
 deriving (Show)

data Type = Int | Real | Error
  deriving (Show)

data Type' = Int' | Real' | Error' ErrorType
  deriving (Show)

data ErrorType = ErrorInitExpr PIdent Type' Type' | ErrorNotInitVar 
  deriving(Show) 

initVarError = ErrorInitExpr

type Loc = (Int,Int)

type MonState = State EnvEntry

startState = DMap.empty


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

typeCheckerDeclaration x = case x of
    NoAssgmDec identifiers _ types -> typeCheckerIdentifiers identifiers types
    AssgmDec identifiers _ exp -> typeCheckerIdentifiersWithExpression identifiers Nothing exp
    AssgmTypeDec identifiers _ types _ exp -> typeCheckerIdentifiersWithExpression identifiers (Just types) exp

typeCheckerIdentifiers identifiers types = 
   mapM_ (typeCheckerIdentifier (convertTypeSpecToTypeInferred types)) identifiers
    
typeCheckerIdentifiersWithExpression identifiers types exp = do
  env <- get
  case types of
    Nothing -> mapM_ (typeCheckerIdentifier (typeCheckerExpression env exp)) identifiers
    Just defineType -> mapM_ (typeCheckerIdentifier (supdecl (convertTypeSpecToTypeInferred defineType) (typeCheckerExpression env exp))) identifiers

-- typechecking nel caso di dichiarazioni senza inizializzazione
typeCheckerIdentifier types (PIdent ((line,column),identifier)) = do
  env <-get
  put (DMap.insert identifier (Variable (line,column) types) env)
  get

typeCheckerExpression env expression = case expression of
  Eplus e1 plus e2 -> sup (typeCheckerExpression env e1) (typeCheckerExpression env e2)
  Evar (PIdent (_, identifier)) -> getVarType identifier env
  Econst (Eint _) -> Int
  Econst (Efloat _) -> Real

-- typechecking nel caso di dichiarazioni con inizializzazione
--typeCheckerGlobalDecls'' (t:types) (InitDecl (NoPointer (Name (Ident ((l,c),name)))) (InitExpr expr) ) = do
--    env <-get
--    put (DMap.insert name (Variable (l,c) ((supdecl t (typeOf env expr)):[]) ) env)
--    get

getVarType var env = case DMap.lookup var env of
    Just (Variable _ t) -> t
    Nothing -> Error

-- infer del tipo fra due tipi. Da capire meglio cosa fare in caso di errore.
sup Int Int = Int
sup Int Real = Real
sup Real Int = Real
sup Real Real = Real
sup Error _ = Error
sup _ Error = Error


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
