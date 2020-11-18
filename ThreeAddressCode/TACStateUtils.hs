module ThreeAddressCode.TACStateUtils where

import ThreeAddressCode.TAC
import Checker.BPTree
import Control.Monad.Trans.State
import Utils.Type


type TacMonad a = State ([TACEntry], Int, BPTree BP, [SequenceLazyEvalLabels], Maybe Label,  [TACEntry]) a


startTacState bpTree = ([],  0, bpTree, [], Nothing, [])

newtemp :: TacMonad String
newtemp = do
  (_tac, k , _bp, _labels, _label, _ft) <- get
  put (_tac, k + 1 , _bp, _labels, _label, _ft)
  return $ int2AddrTempName k


newlabel pos ty = return ("L", pos, ty)

newlabelFALL pos ty = return ("FALL",pos, ty)

popLabel :: TacMonad (Maybe Label)
popLabel = do
  (_tac, k , _bp, _labels, label, _ft) <- get
  put (_tac , k , _bp, _labels, Nothing, _ft)
  return label

pushLabel newLabel = do
  (_tac , k , _bp, _ls, oldLabel, _ft) <- get
  case oldLabel of
      Nothing -> do
          put (_tac , k , _bp, _ls,Just newLabel, _ft)
          return newLabel
      Just label -> return label

pushFunTacs tacEntries = do
  (tac,_t, _b, _labels, _label, _ft ) <- get
  put (tac, _t,_b, _labels, _label, tacEntries ++ _ft)
  get

popFunTacs :: TacMonad [TACEntry]
popFunTacs = do
  (_tac, _t,_b, _labels, _label, ft) <- get
  put (_tac, _t,_b, _labels, _label, [])
  return ft


addTacEntries tacEntry (tac, _t, _b, _labels, _label, _ft ) =
    (tacEntry ++ tac, _t,_b, _labels, _label, _ft)

addTacEntry tacEntry (tac, _t, _b, _labels, _label, _ft) =
    (tacEntry:tac, _t,_b, _labels, _label, _ft)

--addIfSimpleLabels :: Label -> Label -> Label -> (a, b, c, d, [SequenceControlLabel], f, g) -> (a, b, c, d, [SequenceControlLabel], f, g)
addIfSimpleLabels ltrue lfalse lbreak (_tac, _t, _b, _labels, _label,_ft ) =
    (_tac, _t,_b, SequenceLazyEvalLabels ltrue lfalse lbreak:_labels, _label,_ft)

popSequenceControlLabels :: TacMonad SequenceLazyEvalLabels
popSequenceControlLabels = do
    (_tac, _t, _b, ifLabels, _label, _ft ) <- get
    put (_tac, _t, _b,tail ifLabels, _label, _ft )
    return $ head ifLabels

setSequenceControlLabels a = do
  (_tac, _t, _b, ifLabels, _label, _ft ) <- get
  put (_tac, _t, _b, a:ifLabels, _label, _ft )

isLabelFALL (name,_,_) = name == "FALL"

getLabelFromMaybe (Just (name,pos)) = (name,pos)

int2AddrTempName k = "t" ++ show k
