module ThreeAddressCode.TACGenerator where
import ThreeAddressCode.TAC

import AbsChapel
import Control.Monad.Trans.State
import qualified Data.Map as DMap

startTacState bpTree = ([], 0, bpTree)

tacGenerator (Progr p) = tacGeneratorModule p

tacGeneratorModule (Mod m) = do
  tacGeneratorExt m
  get

tacGeneratorExt (x:xs) = case x of
    ExtDecl (Decl decMode declList _ ) -> do
        mapM_ tacGeneratorDeclaration declList
        tacGeneratorExt xs
    ExtFun fun -> -- do
        --tacGeneratorFunction fun
        tacGeneratorExt xs

tacGeneratorDeclaration x = do
  environment <- get
  case x of
    NoAssgmDec ids _colon types -> get
    AssgmDec ids assignment exp ->
      let temp = tacGeneratorDeclExpression environment exp in do
            tacGeneratorIdentifiers temp ids
            get
    AssgmTypeDec ids _colon types assignment exp -> get
      --let DataChecker ty errors = typeCheckerDeclExpression environment exp in do
      --      typeCheckerIdentifiers ids (Just assignment) (convertTypeSpecToTypeInferred types) ty
      --      modify $ addErrorsCurrentNode errors
      --      get
    NoAssgmArrayFixDec ids _colon array -> get --typeCheckerIdentifiersArray ids Nothing array Infered Infered
    NoAssgmArrayDec ids _colon array types -> get --typeCheckerIdentifiersArray ids  Nothing array (convertTypeSpecToTypeInferred types) Infered
    AssgmArrayTypeDec ids _colon array types assignment exp ->  get
      --let DataChecker ty errors = typeCheckerDeclExpression environment exp in do
       --     typeCheckerIdentifiersArray ids (Just assignment) array (convertTypeSpecToTypeInferred types) ty
       --    modify $ addErrorsCurrentNode errors
        --    get
    AssgmArrayDec ids _colon array assignment exp ->  get
     -- let DataChecker ty errors = typeCheckerDeclExpression environment exp in do
       --     typeCheckerIdentifiersArray ids (Just assignment) array Infered ty
         --   modify $ addErrorsCurrentNode errors
         --   get

tacGeneratorIdentifiers = map tacGeneratorIdentifier
tacGeneratorIdentifier temp id = do
    modify $ addTacEntry $ TACEntry Nothing (Nullary (Temp temp (-1,-1)))
    get

addTacEntry tacEntry env@(tac, _t, _b ) =
    (tacEntry:tac, _t,_b)


