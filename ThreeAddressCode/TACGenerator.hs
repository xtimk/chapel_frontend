module ThreeAddressCode.TACGenerator where
import ThreeAddressCode.TAC

import AbsChapel
import Control.Monad.Trans.State
import qualified Data.Map as DMap

startTacState bpTree = ([], 0, bpTree)

tacGenerator (Progr p) = tacGeneratorModule p

tacGeneratorModule (Mod m) = do
  typeCheckerExt m
  get

tacGeneratorExt [] = get
typeCheckerExt (x:xs) = case x of
    ExtDecl (Decl decMode declList _ ) -> get
    ExtFun fun -> get