module Checker.SupTable where

import Checker.SymbolTable
import AbsChapel


--Infered 
sup mode id loc Infered ty = DataChecker ty []
sup mode id loc ty Infered= DataChecker ty []
--Pointer
sup mode id loc (Pointer _p) Int = DataChecker (Pointer _p) []
sup mode id loc (Pointer _p) ty = DataChecker (Error (Just _p)) [ErrorChecker loc $ ErrorCantOpToAddress ty]
sup mode id loc Int (Pointer _p) = DataChecker (Pointer _p) []
sup mode id loc ty (Pointer _p) = DataChecker (Error (Just _p)) [ErrorChecker loc $ ErrorCantOpToAddress ty]
--Int 
sup mode id loc ty1@Int ty2@Int = case mode of 
  SupBool -> DataChecker Bool []
  _ -> DataChecker Int []
sup mode id loc ty1@Int ty2@Real = case mode of
  SupDecl -> DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  SupBool -> DataChecker Bool []
  Sup -> DataChecker Real []
sup mode id loc ty1@Int ty2@Char = case mode of
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ -> DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
sup mode id loc ty1@Int ty2@String = case mode of
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ -> DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
sup mode id loc ty1@Int ty2@Bool = case mode of
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ -> DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
--Real
sup mode id loc ty1@Real ty2@Int = case mode of
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  SupBool -> DataChecker Bool []
  _ -> DataChecker Real []
sup mode id loc ty1@Real ty2@Real = case mode of 
  SupBool -> DataChecker Bool []
  _ -> DataChecker Real []
sup mode id loc ty1@Real ty2@Char = case mode of 
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ ->  DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
sup mode id loc ty1@Real ty2@String = case mode of 
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ ->  DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
sup mode id loc ty1@Real ty2@Bool = case mode of 
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ ->  DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
--Char
sup mode id loc ty1@Char ty2@Int = case mode of
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  SupBool -> DataChecker Bool []
  _ -> DataChecker Char []
sup mode id loc ty1@Char ty2@Real = case mode of 
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ ->  DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
sup mode id loc ty1@Char ty2@Char = case mode of 
  SupBool -> DataChecker Bool []
  _ -> DataChecker Char []
sup mode id loc ty1@Char ty2@String = case mode of 
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  SupDecl -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  SupBool -> DataChecker Bool []
  _ ->  DataChecker String []
sup mode id loc ty1@Char ty2@Bool = case mode of 
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ ->  DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
--String
sup mode id loc ty1@String ty2@Int = case mode of
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ -> DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
sup mode id loc ty1@String ty2@Real = case mode of 
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ ->  DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
sup mode id loc ty1@String ty2@Char = case mode of 
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  SupBool -> DataChecker Bool []
  _ -> DataChecker String []
sup mode id loc ty1@String ty2@String = case mode of 
  SupBool -> DataChecker Bool []
  _ ->  DataChecker String []
sup mode id loc ty1@String ty2@Bool = case mode of 
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ ->  DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
--Bool
sup mode id loc ty1@Bool ty2@Int = case mode of
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ -> DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
sup mode id loc ty1@Bool ty2@Real = case mode of 
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ ->  DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
sup mode id loc ty1@Bool ty2@Char = case mode of 
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ ->  DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
sup mode id loc ty1@Bool ty2@String = case mode of 
  SupFun -> DataChecker (Error Nothing ) [ErrorChecker loc $ ErrorCalledProcWithWrongTypeParam ty1 ty2]
  _ ->  DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]
sup mode id loc ty1@Bool ty2@Bool =  DataChecker Bool []
--Error
sup mode id loc (Error ty1) (Error ty2 ) = DataChecker Real []
sup mode id loc e1@(Error _ ) _ =  DataChecker e1 []
sup mode id loc ty e1@(Error _ ) =  case mode of  
  SupDecl -> DataChecker ty []
  _ -> DataChecker e1 []
--Array
sup mode id loc ar1@(Array typesFirst dimensionFirst) ar2@(Array typesSecond _) = 
  let DataChecker types errors = sup mode id  loc typesFirst typesSecond;
      errorConverted = map (errorIncompatibleTypesChange ar1 ar2) errors in 
  case types of 
    err@(Error _ ) -> DataChecker err errorConverted
    _ -> DataChecker (Array types dimensionFirst) errorConverted
sup mode id loc array@(Array _ _) types = case mode of
  SupDecl -> DataChecker array [ErrorChecker loc $ ErrorIncompatibleDeclTypes id array types]
  _ -> DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes "" array types]
sup mode id loc types array@(Array _ _) = case mode of 
  SupDecl -> DataChecker types [ErrorChecker loc $ ErrorIncompatibleDeclTypes id types array]
  _ -> DataChecker (Error Nothing) [ErrorChecker loc $ ErrorIncompatibleDeclTypes "" types array]


errorIncompatibleTypesChange ty1 ty2 (ErrorChecker loc (ErrorIncompatibleDeclTypes id array types)) = 
  ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2
errorIncompatibleTypesChange ty1 ty2 error = error

convertTypeSpecToTypeInferred Tint {} = Int
convertTypeSpecToTypeInferred Treal {} = Real
convertTypeSpecToTypeInferred Tchar {} = Char
convertTypeSpecToTypeInferred Tstring {} = String
convertTypeSpecToTypeInferred Tbool {} = Bool

convertMode PRef {} = Ref