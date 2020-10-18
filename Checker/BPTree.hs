module Checker.BPTree where

import Checker.SymbolTable
import AbsChapel

data BPTree a = Void | Node {
    id :: String,
    val :: a,
    parentID :: Maybe String,
    children :: [BPTree a]
} deriving (Show)

data BP = BP {
    symboltable :: SymbolTable,
    statemets :: [Statement]
} deriving (Show)

findNodeById searchedId tree@(Node id val parentID children) = if searchedId == id 
    then tree
    else head $ map (findNodeById searchedId) children

getParentID (Node id val parentID children) = parentID

addChild actualNode@(Node idActual val parentId childrenActualNode) childNodeToAdd Void = Void
addChild actualNode@(Node idActual val parentId childrenActualNode) childNodeToAdd entireTree@(Node idEntire _a _b children)  = if idEntire == idActual
    then Node idActual val parentId (reverse (childNodeToAdd:childrenActualNode))
    else Node idEntire _a _b (map (addChild actualNode childNodeToAdd) children)

createChild identifier tree@(Node id val parent children) = Checker.BPTree.Node {Checker.BPTree.id = identifier, 
                                                                   parentID = Just id, 
                                                                   val = "fun", 
                                                                   children = []}

gotoParent tree (Node id val parent children) = case parent of
    Nothing -> Void
    Just parentReal -> findNodeById parentReal tree