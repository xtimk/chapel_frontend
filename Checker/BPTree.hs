module Checker.BPTree where

import Checker.SymbolTable
import AbsChapel

import qualified Data.Map as DMap

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

findNodeById searchedId tree = findNode (bst2list tree)
    where
        findNode [] = Void
        findNode (Void:xs) = findNode xs
        findNode (actualNode@(Node idActual val parentId childrenActualNode):xs) = if searchedId == idActual
            then actualNode
            else findNode xs

    

getParentID (Node id val parentID children) = parentID

setSymbolTable sym tree@(Node id (BP _ statements) parent children) = Checker.BPTree.Node {Checker.BPTree.id = id,
                                                                   val = BP {symboltable = sym, statements = statements}, 
                                                                   parentID = parent, 
                                                                   children = children}

updateTree _ Void = Void
updateTree actualNode@(Node idActual val parentId childrenActualNode) entireTree@(Node idEntire _a _b children) = if idEntire == idActual
    then actualNode
    else Node idEntire _a _b (map (updateTree actualNode) children)



addChild actualNode@(Node idActual val parentId childrenActualNode) childNodeToAdd Void = Void
addChild actualNode@(Node idActual val parentId childrenActualNode) childNodeToAdd entireTree@(Node idEntire _a _b children)  = if idEntire == idActual
    then Node idActual val parentId (reverse (childNodeToAdd:childrenActualNode))
    else Node idEntire _a _b (map (addChild actualNode childNodeToAdd) children)

createChild identifier tree@(Node id val parent children) = Checker.BPTree.Node {Checker.BPTree.id = identifier, 
                                                                   parentID = Just id, 
                                                                   val = BP {symboltable = DMap.empty, statements = []}, 
                                                                   children = []}

gotoParent tree (Node id val parent children) = case parent of
    Nothing -> Void
    Just parentReal -> findNodeById parentReal tree



bst2list = bsttolist []
    where
        bsttolist xs Void = xs
        bsttolist xs x@(Node _ _ _ []) = x:xs
        bsttolist xs x@(Node _ _ _ children) =  x : concatMap (bsttolist xs) children