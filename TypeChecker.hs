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
    AssgmDec identifiers _ exp -> typeCheckerIdentifiersWithExpression identifiers Nothing (Just exp)
    AssgmTypeDec identifiers _ types _ exp -> typeCheckerIdentifiersWithExpression identifiers (Just types) (Just exp)

typeCheckerIdentifiers identifiers types = 
   mapM_ (typeCheckerIdentifier types) identifiers
    
typeCheckerIdentifiersWithExpression identifiers types exp = case types of
    Nothing -> mapM_ (typeCheckerIdentifier (typeCheckerExpression exp)) identifiers
    Just defineType -> mapM_ (typeCheckerIdentifier (typeCheckerExpression exp)) identifiers

-- typechecking nel caso di dichiarazioni senza inizializzazione
typeCheckerIdentifier types (PIdent ((line,column),identifier)) = do
  env <-get
  put (DMap.insert identifier (Variable (line,column) (convertTypeSpecToTypeInferred types)) env)
  get

typeCheckerExpression expression = Tint (PInt ((10,0), "ciao"))
  
-- typechecking nel caso di dichiarazioni con inizializzazione
--typeCheckerGlobalDecls'' (t:types) (InitDecl (NoPointer (Name (Ident ((l,c),name)))) (InitExpr expr) ) = do
--    env <-get
--    put (DMap.insert name (Variable (l,c) ((supdecl t (typeOf env expr)):[]) ) env)
--    get

-- trova il tipo di una espressione
--typeOf env expr = case expr of
--    Econst ((l,c),Eint (_,value)) -> Int
--    Econst ((l,c),Efloat (_,value)) -> Float
--    Econst ((l,c),Echar (_,value)) -> Char
--    Eplus e1 e2 -> sup (typeOf env e1) (typeOf env e2)
--    Evar (_,Ident (_, name)) -> getVarType name env


--getVarType var env = case (DMap.lookup var env) of
--    Just (Variable _ (t:ts)) -> t
--    Nothing -> Error

-- incolla in ghci per testare funzionamento funzione typeOf
-- env = DMap.fromList [("x",Variable (1,5) [Int]),("y",Variable (2,5) [Int])]
-- typeOf env (Econst ((3,11),Efloat ((3,11),20.4)))
-- typeOf env (Eplus (Econst ((2,9),Eint ((2,9),3))) (Evar ((2,13),Ident ((2,13),"x"))))
-- typeOf env (Eplus (Econst ((2,9),Eint ((2,9),3))) (Evar ((2,13),Ident ((2,13),"g"))))


-- infer del tipo fra due tipi. Da capire meglio cosa fare in caso di errore.
--sup Int Int = Int
--sup Int Float = Float
--sup Float Int = Float
--sup Float Float = Float
--sup Error _ = Error
--sup _ Error = Error

-- infer del tipo nel caso di dichiarazioni con inizializzazione della variabile
-- e' leggermente diversa dalla sup definita sopra, sotto un commento al riguardo.
--supdecl Int Int = Int
-- errore perche' questo corrisponde a codice del tipo: int x = 1.3; che deve ovviamente dare errore di tipo
--supdecl Int Float = Error
--supdecl Float Int = Float
--supdecl Float Float = Float
--supdecl Error _ = Error
--supdecl _ Error = Error


convertTypeSpecToTypeInferred (Tint _) = Int
convertTypeSpecToTypeInferred (Treal _) = Real
--convertTypeSpecToTypeInferred (Type Tchar) = Char

--evalState (typeChecker example2 ) startState 
example2 = Progr (Mod [ExtDecl (Decl (PVarMode (PVar ((1,1),"var"))) [NoAssgmDec [PIdent ((1,5),"u")] (PColon ((1,6),":")) (Tint (PInt ((1,8),"int")))] (PSemicolon ((1,12),";"))),ExtDecl (Decl (PVarMode (PVar ((2,1),"var"))) [AssgmTypeDec [PIdent ((2,5),"b")] (PColon ((2,6),":")) (Tint (PInt ((2,8),"int"))) (PAssignmEq ((2,12),"=")) (Econst (Eint (PInteger ((2,14),"5"))))] (PSemicolon ((2,15),";"))),ExtDecl (Decl (PVarMode (PVar ((3,1),"var"))) [AssgmDec [PIdent ((3,5),"n")] (PAssignmEq ((3,7),"=")) (Econst (Eint (PInteger ((3,9),"34"))))] (PSemicolon ((3,11),";")))])
