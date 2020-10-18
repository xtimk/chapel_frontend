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
                   showTree 2 tree
                   putStrLn "\n\n ** After Type Checker **\n\n"
                   print (evalState (typeChecker tree) startState)

                   putStrLn "\n\n ** SYMBOL TABLE **"
                   print (getSymTable (evalState (typeChecker tree) startState))

                   putStrLn "\n\n ** TREE **"
                   print (getTree (evalState (typeChecker tree) startState))

                   exitSuccess


getSymTable (x,_,_) = x
getTree (_,x,_) = x

