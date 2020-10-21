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
    errors :: [ErrorChecker]
} deriving (Show)

findNodeById searchedId tree = findNode (bp2list tree)
    where
        findNode [] = Checker.BPTree.Void
        findNode (Checker.BPTree.Void:xs) = findNode xs
        findNode (actualNode@(Node idActual val parentId childrenActualNode):xs) = if searchedId == idActual
            then actualNode
            else findNode xs

    

getParentID (Node id val parentID children) = parentID


getSymbolTable tree@(Node _ (BP symboltable _ _) _ _) =  symboltable
setSymbolTable sym tree@(Node id (BP _ statements errors) parent children) = Checker.BPTree.Node {Checker.BPTree.id = id,
                                                                   val = BP {symboltable = sym, statements = statements, errors = errors}, 
                                                                   parentID = parent, 
                                                                   children = children}
              
--addEntryTree identifier entry identifierNode tree = updateTree (addEntryNode identifier entry (findNodeById identifierNode tree)) tree
addEntryNode identifier entry tree@(Node id (BP symboltable statements errors) parent children) = 
    Checker.BPTree.Node {Checker.BPTree.id = id,
    val = BP {symboltable = DMap.insert identifier (identifier, entry) symboltable, statements = statements, errors = errors}, 
    parentID = parent, 
    children = children}

--addErrorTree error identifierNode tree = updateTree (addErrorNode error (findNodeById identifierNode tree)) tree
addErrorNode errorChecker tree@(Node id (BP symboltable statements errors) parent children) = 
    Checker.BPTree.Node {Checker.BPTree.id = id,
    val = BP {symboltable = symboltable, statements = statements, errors = errorChecker:errors}, 
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

createChild identifier tree@(Node id val parent children) = Checker.BPTree.Node {Checker.BPTree.id = identifier, 
                                                                   parentID = Just id, 
                                                                   val = BP {symboltable = DMap.empty, statements = [], errors = []}, 
                                                                   children = []}

gotoParent tree (Node id val parent children) = case parent of
    Nothing -> Checker.BPTree.Void
    Just parentReal -> findNodeById parentReal tree

--getVarType (PIdent ((l,c), identifier)) (sym, tree, currentNode) = getVarType'  (bpPathToList currentNode (updateTree(setSymbolTable sym (findNodeById currentNode tree)) tree) ) 
--     where
--         getVarType' [] = Checker.SymbolTable.ErrorVarNotDeclared (l,c) identifier
--         getVarType' ((Node _ (BP actualSym _) _ _):xs) = case DMap.lookup identifier actualSym of
--             Just (_,Variable _ t) -> t
--             Just (_,Function _ _ t) -> t
--             Nothing -> getVarType' xs

getVarType (PIdent ((l,c), identifier)) (sym, tree, currentNode) = 
    let symtable = uniteSymTables $ bpPathToList currentNode tree in
        case DMap.lookup identifier symtable of
            Just (_,Variable _ _ t) -> t
            Just (_,Function _ _ t) -> t
            Nothing -> Checker.SymbolTable.Error (Checker.SymbolTable.ErrorVarNotDeclared (l,c) identifier)       

uniteSymTables = foldr (\y@(Node _ (BP sym _ _errors) _ _) x -> DMap.union sym x ) DMap.empty

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