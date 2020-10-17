module Checker.BPTree where

import Checker.SymbolTable

data BPTree  = Void | Node {
    val :: BP,
    children :: [BPTree],
    parent :: BPTree
}

data BP = BP {
    symboltable :: SymbolTable,
    statemets :: [String]
}