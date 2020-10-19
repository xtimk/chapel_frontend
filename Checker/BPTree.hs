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
    statements :: [Statement]
} deriving (Show)

findNodeById searchedId tree = findNode (bp2list tree)
    where
        findNode [] = Checker.BPTree.Void
        findNode (Checker.BPTree.Void:xs) = findNode xs
        findNode (actualNode@(Node idActual val parentId childrenActualNode):xs) = if searchedId == idActual
            then actualNode
            else findNode xs

    

getParentID (Node id val parentID children) = parentID

setSymbolTable sym tree@(Node id (BP _ statements) parent children) = Checker.BPTree.Node {Checker.BPTree.id = id,
                                                                   val = BP {symboltable = sym, statements = statements}, 
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
                                                                   val = BP {symboltable = DMap.empty, statements = []}, 
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
            Just (_,Variable _ t) -> t
            Just (_,Function _ _ t) -> t
            Nothing -> Checker.SymbolTable.ErrorVarNotDeclared (l,c) identifier            

uniteSymTables = foldr (\y@(Node _ (BP sym _) _ _) x -> DMap.union sym x ) DMap.empty

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