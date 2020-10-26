module Checker.BPTree where

import Checker.SymbolTable
import AbsChapel
import Prelude hiding (id)
import qualified Data.Map as DMap

import Data.List

data BPTree a = Void | Node {
    id :: String,
    val :: a,
    parentID :: Maybe String,
    children :: [BPTree a]
} deriving (Show)

data BP = BP {
    symboltable :: SymbolTable,
    statements :: [Statement],
    errors :: [ErrorChecker],
    blocktype :: BlkType
} deriving (Show)

findNodeById searchedId tree = findNode (bp2list tree)
    where
        findNode [] = Checker.BPTree.Void
        findNode (Checker.BPTree.Void:xs) = findNode xs
        findNode (actualNode@(Node idActual val parentId childrenActualNode):xs) = if searchedId == idActual
            then actualNode
            else findNode xs

getBlkType tree current_id = 
    let node = findNodeById current_id tree in
        case (getBlkTypeSimple node) of
            ProcedureBlk -> ProcedureBlk
            ExternalBlk -> ExternalBlk
            _otherwhise -> getBlkType tree (getParentID node)

getBlkTypeSimple tree@(Node id (BP symboltable statements errors blocktype) parent children) = blocktype
getParentID tree@(Node id (BP symboltable statements errors blocktype) parent children) = 
    case parent of
        Just p -> p
        -- il caso nothing non serve, non e' mai raggiungibile dalla getBlkType
        


-- getParentID (Node id val parentID children) = parentID

modErrorsPos pos = map (modErrorPos pos)

modErrorPos (l,c) (ErrorChecker _oldloc a) = ErrorChecker (l,c) a

getExprDeclPos (ExprDecArray (ArrayInit _ (e:exps) _)) = getExprDeclPos e
getExprDeclPos (ExprDec exp) = getExpPos exp 

getExpPos exp = case exp of
    Evar (PIdent ((l,c),_)) -> (l,c)
    Estring (PString ((l,c),_)) -> (l,c)
    Econst (Efloat (PDouble ((l,c),_))) -> (l,c)
    Econst (Echar (PChar ((l,c),_))) -> (l,c)
    Econst (Eint (PInteger ((l,c),_))) -> (l,c)
    EAss e1 _ _ -> getExpPos e1
    Elor e1 _ _ -> getExpPos e1
    Eland e1 _ _ -> getExpPos e1
    Ebitand e1 _ _ -> getExpPos e1
    Eeq e1 _ _ -> getExpPos e1
    Eneq e1 _ _ -> getExpPos e1
    Elthen e1 _ _ -> getExpPos e1
    Egrthen e1 _ _ -> getExpPos e1
    Ele e1 _ _ -> getExpPos e1
    Ege e1 _ _ -> getExpPos e1
    Eplus e1 _ _ -> getExpPos e1
    Eminus e1 _ _ -> getExpPos e1
    Etimes e1 _ _ -> getExpPos e1
    Ediv e1 _ _ -> getExpPos e1
    Emod e1 _ _ -> getExpPos e1
    Epreop _ e1 -> getExpPos e1
    Earray e1 _ -> getExpPos e1
    InnerExp _ e1 _ -> getExpPos e1


getSymbolTable tree@(Node _ (BP symboltable _ _ blocktype) _ _) =  symboltable
setSymbolTable sym tree@(Node id (BP _ statements errors blocktype) parent children) = 
    Checker.BPTree.Node {Checker.BPTree.id = id,
    val = BP {symboltable = sym, statements = statements, errors = errors, blocktype = blocktype}, 
    parentID = parent, 
    children = children}
              
addEntryNode identifier entry tree@(Node id (BP symboltable statements errors blocktype) parent children) = 
    Checker.BPTree.Node {Checker.BPTree.id = id,
    val = BP {symboltable = DMap.insert identifier (identifier, entry) symboltable, statements = statements, errors = errors, blocktype = blocktype}, 
    parentID = parent, 
    children = children}

addErrorsCurrentNode errors (_s,_e,tree,currentId) = (_s,_e,updateTree (addErrorsNode (findNodeById currentId tree) errors) tree, currentId)

addErrorsNode :: (Foldable t0) => BPTree BP -> t0 ErrorChecker -> BPTree BP
addErrorsNode = foldr addErrorNode
addErrorNode errorChecker tree@(Node id (BP symboltable statements errors blocktype) parent children) = 
    Checker.BPTree.Node {Checker.BPTree.id = id,
    val = BP {symboltable = symboltable, statements = statements, errors = errorChecker:errors, blocktype = blocktype}, 
    parentID = parent, 
    children = children}

updateTree _ Checker.BPTree.Void = Checker.BPTree.Void
updateTree actualNode@(Node idActual val parentId childrenActualNode) entireTree@(Node idEntire _a _b children) = if idEntire == idActual
    then actualNode
    else Node idEntire _a _b (map (updateTree actualNode) children)

addChild actualNode@(Node idActual val parentId childrenActualNode) childNodeToAdd Checker.BPTree.Void = Checker.BPTree.Void
addChild actualNode@(Node idActual val parentId childrenActualNode) childNodeToAdd entireTree@(Node idEntire _a _b children)  = if idEntire == idActual
    then Node idActual val parentId (reverse (childNodeToAdd:childrenActualNode))
    else Node idEntire _a _b (map (addChild actualNode childNodeToAdd) children)

createChild identifier blkType tree@(Node id val parent children) = Checker.BPTree.Node 
    {Checker.BPTree.id = identifier, 
    parentID = Just id, 
    val = BP {symboltable = DMap.empty, statements = [], errors = [], blocktype = blkType}, 
    children = []}

gotoParent tree (Node id val parent children) = case parent of
    Nothing -> Checker.BPTree.Void
    Just parentReal -> findNodeById parentReal tree

getVarType (PIdent ((l,c), identifier)) (_,_,tree,currentNode) = 
    let symtable = uniteSymTables $ bpPathToList currentNode tree in
        case DMap.lookup identifier symtable of
            Just (_,Variable _ _ t) -> DataChecker t []
            Just (_,Function _ _ t) -> DataChecker t []
            Nothing -> DataChecker (Checker.SymbolTable.Error Nothing []) [ErrorChecker (l,c) $ Checker.SymbolTable.ErrorVarNotDeclared identifier]

uniteSymTables = foldr (\y@(Node _ (BP sym _ _errors _) _ _) x -> DMap.union sym x ) DMap.empty

bpPathToList = bpPathToList' []     
    where
        bpPathToList' xs searchId  tree@(Node id val parent []) = if id == searchId 
            then tree:xs
            else []
        bpPathToList' xs searchId tree@(Node id val parent children) = if id == searchId 
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