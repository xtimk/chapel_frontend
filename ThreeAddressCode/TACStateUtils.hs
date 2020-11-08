module ThreeAddressCode.TACStateUtils where

import ThreeAddressCode.TAC
import Checker.BPTree
import Control.Monad.Trans.State
import Utils.Type


type TacMonad a = State ([TACEntry], Temp, Int, BPTree BP, [SequenceLazyEvalLabels], Maybe Label,  [TACEntry]) a


startTacState bpTree = ([], Temp ThreeAddressCode.TAC.Fix "" (0::Int,0::Int) Int,  0, bpTree, [], Nothing, [])

newtemp :: TacMonad String
newtemp = do
  (_tac, _temp , k , _bp, _labels, _label, _ft) <- get
  put (_tac, _temp , k + 1 , _bp, _labels, _label, _ft)
  return $ int2AddrTempName k

--newlabel :: Loc -> TacMonad Label
newlabel pos = do
  (_tac, _temp , k , _bp, _labels, _label, _ft) <- get
  put (_tac, _temp , k + 1 , _bp, _labels, _label, _ft)
  return $ (int2Label k,pos)

newlabelFALL pos = return $ ("FALL",pos)

popLabel :: TacMonad (Maybe Label)
popLabel = do
  (_tac, _temp , k , _bp, _labels, label, _ft) <- get
  put (_tac, _temp , k , _bp, _labels, Nothing, _ft)
  return label

pushLabel label = do
  (_tac, _temp , k , _bp, _labels, _, _ft) <- get
  put (_tac, _temp , k , _bp, _labels, label, _ft)

  

pushFunTacs tacEntries = do
  (tac,_te, _t, _b, _labels, _label, _ft ) <- get
  put (tac,_te, _t,_b, _labels, _label, tacEntries ++ _ft)
  get

popFunTacs :: TacMonad [TACEntry]
popFunTacs = do
  (_tac,_te, _t,_b, _labels, _label, ft) <- get
  put (_tac,_te, _t,_b, _labels, _label, [])
  return ft


addTacEntries tacEntry (tac,_te, _t, _b, _labels, _label, _ft ) =
    (tacEntry ++ tac,_te, _t,_b, _labels, _label, _ft)

addTacEntry tacEntry (tac,_te, _t, _b, _labels, _label, _ft) =
    (tacEntry:tac,_te, _t,_b, _labels, _label, _ft)

--addIfSimpleLabels :: Label -> Label -> Label -> (a, b, c, d, [SequenceControlLabel], f, g) -> (a, b, c, d, [SequenceControlLabel], f, g)
addIfSimpleLabels ltrue lfalse lbreak (_tac,_te, _t, _b, _labels, _label,_ft ) =
    (_tac,_te, _t,_b, (SequenceLazyEvalLabels ltrue lfalse lbreak):_labels, _label,_ft)

popSequenceControlLabels :: TacMonad (SequenceLazyEvalLabels)
popSequenceControlLabels = do
    (_tac,_te, _t, _b, ifLabels, _label, _ft ) <- get
    case ifLabels of
      -- [] -> return ()
      _ -> do 
        put (_tac,_te, _t, _b,tail ifLabels, _label, _ft )
        return $ head ifLabels

setSequenceControlLabels a = do
  (_tac,_te, _t, _b, ifLabels, _label, _ft ) <- get
  put (_tac,_te, _t, _b, a:ifLabels, _label, _ft )

isLabelFALL (name,pos) = name == "FALL"

getLabelFromMaybe (Just (name,pos)) = (name,pos)

int2AddrTempName k = "t" ++ show k
int2Label k = "L" ++ show k
