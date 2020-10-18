module Checker.BPTree where

import Checker.SymbolTable

data BPTree a = Void | Node {
    id :: String,
    val :: a,
    parentID :: Maybe String,
    children :: [BPTree a]
} deriving (Show)

data BP = BP {
    symboltable :: SymbolTable,
    statemets :: [String]
} deriving (Show)

findNodeById searchedId tree@(Node id val parentID children) = 
--    case searchedId of 
--        Just anId -> 
            case searchedId == id of
                True -> tree
                otherwhise -> head $ map (findNodeById searchedId) children
--        Nothing -> Void

getParentID (Node id val parentID children) = parentID

addChild actualNode@(Node idActual val parentId childrenActualNode) childNodeToAdd Void = Void
addChild actualNode@(Node idActual val parentId childrenActualNode) childNodeToAdd entireTree@(Node idEntire _a _b children)  = case idEntire == idActual of
    True -> Node idActual val parentId (childNodeToAdd:childrenActualNode)
    otherwise -> Node idActual _a _b (map (addChild actualNode childNodeToAdd) children)

createChild identifier tree@(Node id val parent children) = Checker.BPTree.Node {Checker.BPTree.id = identifier, 
                                                                   parentID = Just id, 
                                                                   val = "fun", 
                                                                   children = []}

gotoParent tree (Node id val parent children) = case parent of
    Nothing -> Void
    Just parentReal -> findNodeById parentReal tree