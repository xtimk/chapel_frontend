module Checker.TypeChecker where


import Control.Monad.Trans.State
import qualified Data.Map as DMap
import AbsChapel
import Checker.SymbolTable
import Checker.BPTree
import Data.Maybe
import Debug.Trace

--type Env = DMap.Map PIdent EnvEntry



initVarError = ErrorInitExpr


--type MonState = State EnvEntry 

startState = (DMap.empty, Checker.BPTree.Node {Checker.BPTree.id = "0", val = BP {symboltable = DMap.empty, statements = []}, parentID = Nothing, children = []}, "0")


typeChecker (Progr p) = typeCheckerModule p

typeCheckerModule (Mod m) = typeCheckerExt m

-- funzione per il typeChecking
-- usa la state monad per memorizzare info sui tipi di variabile
-- da fare: al posto dello stato contenente solo la Data.Map var -> tipo
-- implementare stato che e' una coppia del tipo (Data.Map(quella di prima), abstract syntax arricchito con info di tipo)
createId l c name = show l ++ "_" ++ show c ++ "_" ++ name

typeCheckerExt [] = do 
  (sym, tree, actual_id) <- get
  let actualTree = findNodeById actual_id tree in
    put (DMap.empty, updateTree (setSymbolTable sym actualTree) tree, actual_id)
  get
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
  SignNoRet identifier (FunParams _ params _) -> typeCheckerSignature' identifier params NotDeclared
  SignWRet identifier (FunParams _ params _) _ types -> typeCheckerSignature' identifier params (convertTypeSpecToTypeInferred types)

typeCheckerSignature' (PIdent ((line,column),identifier)) params types = do
  (symtable, tree, current_id) <- get
  put (DMap.insert identifier (identifier, Function (line,column) [] types) symtable, tree, current_id )
  (symtable, tree, current_id) <- get
  put (symtable, (updateTree(setSymbolTable symtable (findNodeById current_id tree)) tree), current_id)
  get 

typeCheckerBody identifier (BodyBlock  _ xs _  ) = do
  -- create new child for the blk and enter in it
  (_sym, tree, current_id) <- get
  let actualNode = findNodeById current_id tree in
    put (DMap.empty, addChild actualNode (createChild identifier actualNode) tree, identifier)
  -- process body
  mapM_ typeCheckerBody' xs
  -- exit from child and return to parent
  (sym, tree, actual_id) <- get
  let actualTree = findNodeById actual_id tree in
    put (_sym, updateTree (setSymbolTable sym actualTree) tree, current_id)
    -- put (depth, env, DMap.empty, tree, current_id)
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
          Error -> do 
            (sym, tree, current_id) <- get
            get
          otherwhise -> do
            (sym, tree, current_id) <- get
            get
      else do -- caso di lvalue non var
        (sym, tree, current_id) <- get
        -- put (DMap.insert (getAssignOpPos eqsym) (getAssignOpTok eqsym, Assignm (getAssignOpPos eqsym) Error) sym, tree, current_id )
        get
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
  (symtable, tree, current_id) <- get
  put (DMap.insert identifier (identifier, Variable (line,column) types) symtable, tree, current_id )
  (symtable, tree, current_id) <- get
  put (symtable, (updateTree(setSymbolTable symtable (findNodeById current_id tree)) tree), current_id)
  get

--(updateTree(setSymbolTable sym tree) tree)

typeCheckerExpression environment@(_sym, _tree, current_id) expression = case expression of
  Eplus e1 plus e2 -> sup (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
  Eminus e1 minus e2 -> sup (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
  Ediv e1 div e2 -> sup (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
  Etimes e1 div e2 -> sup (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
  InnerExp _ e _ -> typeCheckerExpression environment e
  Elthen e1 pElthen e2 -> supBool (typeCheckerExpression environment e1) (typeCheckerExpression environment e2) 
  Evar identifier -> getVarType identifier environment
  EAss e1 _eqsym e2 -> supdecl (typeCheckerExpression environment e1) (typeCheckerExpression environment e2)
  Econst (Eint _) -> Int
  Econst (Efloat _) -> Real
  -- todo: aggiungere tutti i casi degli operatori esistenti


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
