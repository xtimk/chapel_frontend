-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexChapel
import ParChapel
import SkelChapel
import PrintChapel
import AbsChapel
import qualified Data.Map as DMap
import Checker.TypeChecker
import Control.Monad.Trans.State
import Checker.ErrorPrettyPrinter
import Checker.BPTree
import ThreeAddressCode.TACGenerator
import ThreeAddressCode.TACPrettyPrinter
import ThreeAddressCode.TACStateUtils



import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree
                          exitSuccess


showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pProgram
    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (parseTest) fs

    
parseTest filepath = do
  s <- readFile filepath
  let p = pProgram (myLLexer s) in case p of
    Bad s    -> do putStrLn s
                   exitFailure
    Ok  tree -> do putStrLn "\nParse Successful!"
                   --showTree 2 tree

                   --putStrLn "\n\n ** After Type Checker **\n\n"
                   --print (evalState (typeChecker tree) startState)

                   let bpTree = evalState (typeChecker tree) startState
                       errors = getTreeErrors $ getTree bpTree in do

                    let bp2 = typeCheckerReturns (getTree bpTree) in
                      putStrLn $ show bp2

                    putStrLn "\n\n ** SYMBOL TABLE **"
                    print $ getSymTable bpTree

                    putStrLn "\n\n ** TREE **"
                    print $ getTree bpTree

                    putStrLn "\n\n ** ERRORS **\n"
                    printErrors (tokens s) errors

                    if null errors 
                    then let tac = evalState (tacGenerator tree) (startTacState (getTree bpTree)) in do

                     -- putStrLn "\n\n ** TAC **"
                      --putStrLn $ show tac
                      putStrLn "\n\n ** Pretty TAC **"
                      printTacEntries (getTac tac)
                      exitSuccess
                    else 
                      exitSuccess


typeCheckerReturns bp@(Node _ _ _ children) = andOfAList (map typeCheckerReturnPresence children)

-- typeCheckerReturns' node@(Node id (BP symboltable statements errors blocktype) parent children) = 
--   if blocktype == ProcedureBlk
--     then
--       if typeCheckerReturnPresence node
--         then []
--         else []
--     else map typeCheckerReturns' children

typeCheckerReturnPresence node@(Node id (BP symboltable statements errors blocktype) parent children) = 
  do
    if isAReturn node
      then True
      else orOfAList (map typeCheckerReturnPresence children)
        
isAReturn (Node id (BP symboltable statements errors blocktype) parent children) = 
  if null statements
    then False
    else True

orOfAList :: [Bool] -> Bool
orOfAList = foldr (||) False

andOfAList :: [Bool] -> Bool
andOfAList = foldr (&&) True

getSymTable (x,_,_) = x
getTree (_,x,_) = x

getTac (x,_,_,_,_,_,_) = x


