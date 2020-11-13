{-# LANGUAGE BlockArguments #-}
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

                    putStrLn "\n\n ** SYMBOL TABLE **"
                    print $ getSymTable bpTree

                    putStrLn "\n\n ** TREE **"
                    print $ getTree bpTree

                    putStrLn "\n\n ** ERRORS **\n"
                    let bp2 = typeCheckerReturns (getTree bpTree)
                    printErrors (tokens s) (errors ++ bp2)

                    if null (errors ++ bp2)
                    then let tac = evalState (tacGenerator tree) (startTacState (getTree bpTree)) in do

                     -- putStrLn "\n\n ** TAC **"
                      --putStrLn $ show tac
                      putStrLn "\n\n ** Pretty TAC **"
                      printTacEntries (getTac tac)
                      exitSuccess
                    else 
                      exitSuccess


typeCheckerReturns bp@(Node _ _ _ startnodes) = concat (map typeCheckerReturnPresenceFun startnodes)

typeCheckerReturnPresenceFun :: BPTree BP -> [ErrorChecker]
typeCheckerReturnPresenceFun (node@(Node (funname,(locstart,locend)) (BP _ rets _ ProcedureBlk) _ (children))) = 
  if null rets
    then
      typeCheckerReturnPresence children funname (locstart,locend)
    else
      -- ho quasi finito: 
      -- devo controllare se ci sono altri blocchi funzione nei children o dopo 
      -- e verificare anche quelli
      let declFunsInChildren = filter isNodeAProc children;        
      in
        concat (map typeCheckerReturnPresenceFun declFunsInChildren)

typeCheckerReturnPresenceElse node@(Node id (BP _ rets _ IfElseBlk) _ (children)) funname (locstart, locend) =
  if null rets
    then typeCheckerReturnPresence children funname (locstart, locend)
    else []

getblkStartEndPos (Node (_,(act_locstart, act_locend)) (BP _ rets _ blocktype) _ (children)) = (act_locstart, act_locend)

isNodeAProc node@(Node (_,(act_locstart, act_locend)) (BP _ rets _ blocktype) _ (children)) = blocktype == ProcedureBlk


typeCheckerReturnPresence [] funname (locstart, locend) = [ErrorChecker locstart $ ErrorFunctionWithNotEnoughReturns funname]

typeCheckerReturnPresence (node@(Node (_,(act_locstart, act_locend)) (BP _ rets _ blocktype) _ (children)):xs) funname (locstart, locend) =
  case blocktype of
    IfSimpleBlk -> 
      if null xs
        then [ErrorChecker act_locstart $ ErrorFunctionWithNotEnoughReturns funname]
        else typeCheckerReturnPresence xs funname (act_locstart, act_locend)
    IfThenBlk -> 
      if null rets
        then 
          let errs1 = typeCheckerReturnPresence children funname (act_locstart, act_locend);
              elset = typeCheckerReturnPresenceElse (head xs) funname (getblkStartEndPos (head xs)) in
                case ((null errs1),(null elset)) of
                  (True,True) -> 
                    -- ho quasi finito: 
                    -- devo controllare se ci sono altri blocchi funzione nei children o dopo 
                    -- e verificare anche quelli
                    let declFunsInChildren = filter isNodeAProc children;
                        declFunsAhead = filter isNodeAProc xs
                    in
                      let childrens = concat (map typeCheckerReturnPresenceFun declFunsInChildren)
                          aheads = concat (map typeCheckerReturnPresenceFun declFunsAhead) in
                          childrens ++ aheads
                  _otherwhise -> 
                    -- sono nel caso in cui ho trovato un return nel then o nell'else o da nessuna parte
                    -- non posso affermare che ci siano errori, devo andare avanti con la computazione
                    if null (tail xs)
                      then [ErrorChecker act_locstart $ ErrorFunctionWithNotEnoughReturns funname]
                      else (typeCheckerReturnPresence (tail xs) funname (getblkStartEndPos (head (tail xs))))
        else
          let elset = typeCheckerReturnPresenceElse (head xs) funname (getblkStartEndPos (head xs)) in
            case null elset of
              True -> 
                -- ho quasi finito: 
                -- devo controllare se ci sono altri blocchi funzione nei children o dopo 
                -- e verificare anche quelli
                let declFunsInChildren = filter isNodeAProc children;
                    declFunsAhead = filter isNodeAProc xs
                in
                  let childrens = concat (map typeCheckerReturnPresenceFun declFunsInChildren)
                      aheads = concat (map typeCheckerReturnPresenceFun declFunsAhead) in
                      childrens ++ aheads
              _oth ->
                if null (tail xs)
                  then [ErrorChecker act_locstart $ ErrorFunctionWithNotEnoughReturns funname]
                  else (typeCheckerReturnPresence (tail xs) funname (getblkStartEndPos (head (tail xs))))

    ProcedureBlk -> 
        if null rets -- se non ci sono return al blocco base
          then
            let c = typeCheckerReturnPresence children funname (act_locstart, act_locend) in
              case null c of
                True -> 
                  -- ho quasi finito: 
                  -- devo controllare se ci sono altri blocchi funzione nei children o dopo 
                  -- e verificare anche quelli
                  let declFunsInChildren = filter isNodeAProc children;
                      declFunsAhead = filter isNodeAProc xs
                  in
                    let childrens = concat (map typeCheckerReturnPresenceFun declFunsInChildren)
                        aheads = concat (map typeCheckerReturnPresenceFun declFunsAhead) in
                        childrens ++ aheads
                _oth -> [ErrorChecker act_locstart $ ErrorFunctionWithNotEnoughReturns funname]
          else
            -- ho quasi finito: 
            -- devo controllare se ci sono altri blocchi funzione nei children o dopo 
            -- e verificare anche quelli
            let declFunsInChildren = filter isNodeAProc children;
                declFunsAhead = filter isNodeAProc xs
            in
              let childrens = concat (map typeCheckerReturnPresenceFun declFunsInChildren)
                  aheads = concat (map typeCheckerReturnPresenceFun declFunsAhead) in
                  childrens ++ aheads
    WhileBlk -> 
      if null xs
        then [ErrorChecker act_locstart $ ErrorFunctionWithNotEnoughReturns funname]
        else typeCheckerReturnPresence xs funname (act_locstart, act_locend)
    DoWhileBlk -> 
      if null rets
        then
          let errs = typeCheckerReturnPresence children funname (act_locstart, act_locend) in
          case null errs of
            True -> 
              -- ho quasi finito: 
              -- devo controllare se ci sono altri blocchi funzione nei children o dopo 
              -- e verificare anche quelli
              let declFunsInChildren = filter isNodeAProc children;
                  declFunsAhead = filter isNodeAProc xs
              in
                let childrens = concat (map typeCheckerReturnPresenceFun declFunsInChildren)
                    aheads = concat (map typeCheckerReturnPresenceFun declFunsAhead) in
                    childrens ++ aheads
            _otherwhise ->
              if null xs
                then [ErrorChecker locstart $ ErrorFunctionWithNotEnoughReturns funname]
                else typeCheckerReturnPresence xs funname (act_locstart, act_locend)
        else
          -- ho quasi finito: 
          -- devo controllare se ci sono altri blocchi funzione nei children o dopo 
          -- e verificare anche quelli
          let declFunsInChildren = filter isNodeAProc children;
              declFunsAhead = filter isNodeAProc xs
          in
            let childrens = concat (map typeCheckerReturnPresenceFun declFunsInChildren)
                aheads = concat (map typeCheckerReturnPresenceFun declFunsAhead) in
                childrens ++ aheads
    SimpleBlk -> 
      if null rets
        then
          typeCheckerReturnPresence children funname (act_locstart, act_locend)
        else
          -- ho quasi finito: 
          -- devo controllare se ci sono altri blocchi funzione nei children o dopo 
          -- e verificare anche quelli
          let declFunsInChildren = filter isNodeAProc children;
              declFunsAhead = filter isNodeAProc xs
          in
            let childrens = concat (map typeCheckerReturnPresenceFun declFunsInChildren)
                aheads = concat (map typeCheckerReturnPresenceFun declFunsAhead) in
                childrens ++ aheads

getSymTable (x,_,_) = x
getTree (_,x,_) = x

getTac (x,_,_,_,_,_,_) = x


