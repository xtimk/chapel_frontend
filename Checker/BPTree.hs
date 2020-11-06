module Checker.BPTree where

import Checker.SymbolTable
import Utils.Type
import Utils.AbsUtils
import AbsChapel
import Prelude hiding (id)
import qualified Data.Map as DMap
import Checker.ErrorPrettyPrinter
import ThreeAddressCode.TAC

data BPTree a = Void | Node {
    id :: (String, (Loc, Loc)),
    val :: a,
    parentID :: Maybe (String, (Loc, Loc)),
    children :: [BPTree a]
} deriving (Show)

data BP = BP {
    symboltable :: SymbolTable,
    statements :: [Statement],
    errors :: [ErrorChecker],
    blocktype :: BlkType
} deriving (Show)


data BlkType = 
  DoWhileBlk |
  WhileBlk |
  ProcedureBlk |
  ExternalBlk |
  SimpleBlk |
  IfSimpleBlk |
  IfThenBlk |
  IfElseBlk
  deriving (Show)

getTreeErrors tree = foldr (\tree errors -> getErrors tree ++ errors ) [] (bp2list tree)

findNodeById searchedId tree = findNode (bp2list tree)
    where
        findNode [] = Checker.BPTree.Void
        findNode (Checker.BPTree.Void:xs) = findNode xs
        findNode (actualNode@(Node idActual _ _ _):xs) = if searchedId == idActual
            then actualNode
            else findNode xs

findProcedureGetBlkType tree current_id = 
    let node = findNodeById current_id tree in
        case getBlkTypeSimple node of
            ProcedureBlk -> ProcedureBlk
            ExternalBlk -> ExternalBlk
            _otherwhise -> findProcedureGetBlkType tree (getParentID node)

getBlkTypeSimple (Node _ (BP _ _ _ blocktype) _ _) = blocktype

findSequenceControlGetBlkType tree current_id = 
    let node = findNodeById current_id tree in
        case getBlkTypeSimple node of
            WhileBlk -> WhileBlk
            DoWhileBlk -> DoWhileBlk
            IfSimpleBlk -> IfSimpleBlk
            IfThenBlk -> IfThenBlk
            IfElseBlk -> IfElseBlk
            ExternalBlk -> ExternalBlk -- sono arrivato ricorsivamente fino al blocco esterno, lo restituisco, non vado in ricorsione
            _otherwhise -> findSequenceControlGetBlkType tree (getParentID node)

getParentID (Node _ _ parent _) = 
    case parent of
        Just p -> p
        -- il caso nothing non serve, non e' mai raggiungibile dalla getBlkType

modErrorsPos pos = map (modErrorPos pos)

modErrorPos loc (ErrorChecker _oldloc a) = ErrorChecker loc a

modFunRetType identifier tye (Node id (BP symboltable statements errors blocktype) parent children) = 
    Checker.BPTree.Node {Checker.BPTree.id = id,
    val = BP {symboltable = DMap.adjust (alterRetType tye) identifier symboltable, statements = statements, errors = errors, blocktype = blocktype}, 
    parentID = parent, 
    children = children}

modFunAddOverloading identifier vars (Node id (BP symboltable statements errors blocktype) parent children) = 
    Checker.BPTree.Node {Checker.BPTree.id = id,
    val = BP {symboltable = DMap.adjust (addFunVariables vars) identifier symboltable, statements = statements, errors = errors, blocktype = blocktype}, 
    parentID = parent, 
    children = children}

alterRetType tye (s,(Function _a _b _ )) = (s,(Function _a _b tye))
addFunVariables vars (s,(Function _a varOfVar _t )) = (s,(Function _a (vars:varOfVar) _t))

getSymbolTable (Node _ (BP symboltable _ _ _) _ _) =  symboltable
setSymbolTable sym (Node id (BP _ statements errors blocktype) parent children) = 
    Checker.BPTree.Node {Checker.BPTree.id = id,
    val = BP {symboltable = sym, statements = statements, errors = errors, blocktype = blocktype}, 
    parentID = parent, 
    children = children}

getErrors (Node _ (BP _ _ errors _) _ _) = errors
              

addFunctionOnCurrentNode identifier loc variables types (_s,tree,currentId) = 
    let entry = Function loc variables types in
    (_s,updateTree (addEntryNode identifier entry (findNodeById currentId tree)) tree, currentId)
addEntryNode identifier entry (Node id (BP symboltable _st _e _bt) _p _c) = 
    Checker.BPTree.Node {Checker.BPTree.id = id,
    val = BP {symboltable = DMap.insert identifier (identifier, entry) symboltable, statements = _st, errors = _e, blocktype = _bt}, 
    parentID = _p, 
    children = _c}

addErrorsCurrentNode errors (_s,tree,currentId) = (_s,updateTree (addErrorsNode (findNodeById currentId tree) errors) tree, currentId)


addErrorsNode :: (Foldable t0) => BPTree BP -> t0 ErrorChecker -> BPTree BP
addErrorsNode = foldr addErrorNode
addErrorNode errorChecker (Node id (BP symboltable statements errors blocktype) parent children) = 
    Checker.BPTree.Node {Checker.BPTree.id = id,
    val = BP {symboltable = symboltable, statements = statements, errors = errorChecker:errors, blocktype = blocktype}, 
    parentID = parent, 
    children = children}

updateTree _ Checker.BPTree.Void = Checker.BPTree.Void
updateTree actualNode@(Node idActual _ _ _) (Node idEntire  _a _b children) = if idEntire == idActual
    then actualNode
    else Node idEntire _a _b (map (updateTree actualNode) children)

addChild (Node _ _ _ _) _ Checker.BPTree.Void = Checker.BPTree.Void
addChild actualNode@(Node idActual val parentId childrenActualNode) childNodeToAdd (Node idEntire _a _b children)  = if idEntire == idActual
    then Node idActual val parentId (reverse (childNodeToAdd:childrenActualNode))
    else Node idEntire _a _b (map (addChild actualNode childNodeToAdd) children)

createChild identifier blkType locStart locEnd (Node id _ _ _) = Checker.BPTree.Node 
    {Checker.BPTree.id = (identifier,(locStart,locEnd)), 
    parentID = Just id, 
    val = BP {symboltable = DMap.empty, statements = [], errors = [], blocktype = blkType}, 
    children = []}

gotoParent tree (Node _ _ parent _) = case parent of
    Nothing -> Checker.BPTree.Void
    Just parentReal -> findNodeById parentReal tree

getEntry (PIdent ((l,c), identifier)) (_,tree,currentNode) = 
    let symtable = uniteSymTables $ bpPathToList currentNode tree in
        case DMap.lookup identifier symtable of
            Just (_, entry) -> DataChecker (Just entry) []
            Nothing -> DataChecker Nothing [ErrorChecker (l,c) $ ErrorVarNotDeclared identifier]
            
getVarType (PIdent ((l,c), identifier)) (_,tree,currentNode) = 
    let symtable = uniteSymTables $ bpPathToList currentNode tree in
        case DMap.lookup identifier symtable of
            Just (_,Variable _ t) -> DataChecker t []
            Just (_,Function _ _ t) -> DataChecker t []
            Nothing -> DataChecker Error [ErrorChecker (l,c) $ ErrorVarNotDeclared identifier]

getVarTypeTAC (PIdent (loc, identifier)) (_,_,_,tree,_,_)  = 
    let currentNode = getCurIdOfTokenPos loc tree
        symtable = uniteSymTables $ bpPathToList currentNode tree in
        case DMap.lookup identifier symtable of
            Just (_,var) -> var

getFunctionParams identifier paramsPassed env = 
    let Function _ params _ = getVarTypeTAC identifier env in 
        head $ filter (matchParams paramsPassed) params

matchParams [] [] = True
matchParams _ [] = False
matchParams [] _ = False
matchParams  ((Temp _ _ _ y):ys) ((Variable _ ty):xs) = 
    if y == ty
        then matchParams ys xs 
        else False

getFunParams (PIdent (_, identifier)) (_,tree,currentNode) = 
    let symtable = uniteSymTables $ bpPathToList currentNode tree in
        case DMap.lookup identifier symtable of
            Just (_,Function _ params _) -> params

getFunRetType env@(s,tree,current_id) = 
  let (Node (id, _) (BP _ _ _ blkTy) pId _) = findNodeById current_id tree in
    case pId of
      Just parID ->
        case blkTy of
          ProcedureBlk -> getVarType (PIdent ((0,0), id)) env
          _otherwhise -> getFunRetType (s,tree,parID)

getFunNameFromEnv (_s,tree,current_id) = 
  let (Node (id,_) (BP _ _ _ blkTy) (Just parID) _) = findNodeById current_id tree in
    case blkTy of
      ProcedureBlk -> id
      _otherwhise -> getFunNameFromEnv (_s,tree,parID)


setReturnType ty env@(_,_,actualId) = setReturnType' actualId ty env
    where
        setReturnType' actualId ty (_s,tree,current_id) =
            let (Node (id, _) (BP _ _ _ blkTy) (Just parID) _) = findNodeById current_id tree in
                case blkTy of
                ProcedureBlk -> let node = findNodeById parID tree in (_s, updateTree (modFunRetType id ty node) tree , actualId )
                _otherwhise -> setReturnType' actualId ty (_s,tree,parID)


uniteSymTables = foldr (\(Node _ (BP sym _ _errors _) _ _) x -> DMap.union sym x ) DMap.empty

bpPathToList = bpPathToList' []     
    where
        bpPathToList' xs searchId  tree@(Node id _ _ []) = if id == searchId 
            then tree:xs
            else []
        bpPathToList' xs searchId tree@(Node id _ _ children) = if id == searchId 
            then tree:xs
            else bpTakeGoodPath (map (bpPathToList' (tree:xs) searchId  ) children)
                where
                    bpTakeGoodPath [] = []
                    bpTakeGoodPath (x:xs) = if null x
                        then bpTakeGoodPath xs
                        else x

bp2list = bpttolist []
    where
        bpttolist xs Checker.BPTree.Void = xs
        bpttolist xs x@(Node _ _ _ []) = x:xs
        bpttolist xs x@(Node _ _ _ children) =  x : concatMap (bpttolist xs) children


getCurIdOfTokenPos loc (Node id _ _ children) = 
  let r = filter (isIdInTree loc) children in
    if null r
      then id
      else getCurIdOfTokenPos loc (head r)

isIdInTree (l,c)  (Node (_,((lstart,cstart),(lend,cend))) _ _ _) 
  | l < lstart = False
  -- a
  --{
  --}
  | l == lstart && l < lend && c < cstart =  False
  -- a {
  --}
  | l == lstart && l < lend && c >= cstart =  True
  --{ a
  --}
  | l > lstart && l < lend = True
  --{
  -- a
  --}
  | l > lstart && l == lend && c <= cend = True
  --{
  --
  -- a }
  | l == lstart && l == lend && c >= cstart  && c <= cend = True
  --{  a }
  | l == lstart && l == lend && c < cstart = False
  --{   } a
  | otherwise = False
  --{   }
  --a