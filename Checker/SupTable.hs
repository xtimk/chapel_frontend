module Checker.SupTable where

import Checker.ErrorPrettyPrinter
import AbsChapel
import Utils.Type
import Utils.AbsUtils

data SupMode = SupDecl | SupFun | SupBool | SupPlus | SupMinus |  SupArith | SupMod | Sup | SupRet

--Infered 
sup mode id loc Infered ty = DataChecker ty []
sup mode id loc ty Infered = DataChecker ty []
--Pointer
sup mode id loc (Pointer _p1) (Pointer _p2) = let DataChecker ty errors = sup mode id loc _p1 _p2 in
  DataChecker (Pointer ty) errors
sup mode id loc point@(Pointer _p) Int = case mode of 
  SupPlus -> DataChecker (Pointer _p) []
  SupMinus -> DataChecker (Pointer _p) []
  _ -> DataChecker point [ErrorChecker loc $ ErrorCantOpToAddress Int]
sup mode id loc (Pointer _p) ty = DataChecker _p [ErrorChecker loc $ ErrorCantOpToAddress ty]
sup mode id loc Int point@(Pointer _p) = case mode of 
  SupPlus -> DataChecker (Pointer _p) []
  SupMinus -> DataChecker (Pointer _p) []
  _ -> DataChecker point [ErrorChecker loc $ ErrorCantOpToAddress Int]
sup mode id loc ty (Pointer _p) = DataChecker _p [ErrorChecker loc $ ErrorCantOpToAddress ty]
--Void 
sup mode id loc ty1@Void ty2@Int = case mode of 
  SupBool -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _ -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@Void ty2@Real = case mode of
  SupMod ->  createIncompatible id loc ty1 ty2
  SupDecl -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  SupFun -> createIncompatible id loc ty1 ty2
  SupBool -> DataChecker Bool []
  _ -> DataChecker Real []
sup mode id loc ty1@Void ty2@Char = case mode of 
  SupFun -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _ -> DataChecker Void []
sup mode id loc ty1@Void ty2@String = createIncompatible id loc ty1 ty2
sup mode id loc ty1@Void ty2@Bool = createIncompatible id loc ty1 ty2
sup mode id loc ty1@Void ty2@Void = DataChecker Void []
--Int 
sup mode id loc ty1@Int ty2@Int = case mode of 
  SupBool -> DataChecker Bool []
  _ -> DataChecker Int []
sup mode id loc ty1@Int ty2@Real = case mode of
  SupMod ->  createIncompatible id loc ty1 ty2
  SupDecl -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  SupFun -> createIncompatible id loc ty1 ty2
  SupBool -> DataChecker Bool []
  _ -> DataChecker Real []
sup mode id loc ty1@Int ty2@Char = case mode of 
  SupFun -> createIncompatible id loc ty1 ty2
  _ -> DataChecker Int []
sup mode id loc ty1@Int ty2@String = createIncompatible id loc ty1 ty2
sup mode id loc ty1@Int ty2@Bool = createIncompatible id loc ty1 ty2
sup mode id loc ty1@Int ty2@Void = createIncompatible id loc ty1 ty2
--Real
sup mode id loc ty1@Real ty2@Int = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupMod ->  createIncompatible id loc ty1 ty2
  SupBool -> DataChecker Bool []
  _ -> DataChecker Real []
sup mode id loc ty1@Real ty2@Real = case mode of
  SupMod ->  createIncompatible id loc ty1 ty2 
  SupBool -> DataChecker Bool []
  _ -> DataChecker Real []
sup mode id loc ty1@Real ty2@Char = case mode of
  SupRet -> createIncompatibleRet id loc ty1 ty2
  SupFun -> createIncompatible id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@Real ty2@String =  case mode of
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@Real ty2@Bool = case mode of
  SupRet -> createIncompatibleRet id loc ty1 ty2
  SupFun -> createIncompatible id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@Real ty2@Void = createIncompatible id loc ty1 ty2
--Char
sup mode id loc ty1@Char ty2@Int = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupBool -> DataChecker Bool []
  _ -> DataChecker Char []
sup mode id loc ty1@Char ty2@Real = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@Char ty2@Char = case mode of 
  SupBool -> DataChecker Bool []
  _ -> DataChecker Char []
sup mode id loc ty1@Char ty2@String = case mode of 
  SupMod ->  createIncompatible id loc ty1 ty2
  SupFun -> createIncompatible id loc ty1 ty2
  SupDecl -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  SupArith -> createIncompatible id loc ty1 ty2
  SupBool -> DataChecker Bool []
  _ ->  DataChecker String []
sup mode id loc ty1@Char ty2@Bool = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@Char ty2@Void = createIncompatible id loc ty1 ty2
--String
sup mode id loc ty1@String ty2@Int = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@String ty2@Real = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@String ty2@Char = case mode of 
  SupFun -> createIncompatible id loc ty1 ty2
  SupMod ->  createIncompatible id loc ty1 ty2
  SupArith -> createIncompatible id loc ty1 ty2
  SupBool -> DataChecker Bool []
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _ -> DataChecker String []
sup mode id loc ty1@String ty2@String = case mode of
  SupMod ->  createIncompatible id loc ty1 ty2 
  SupArith -> createIncompatible id loc ty1 ty2
  SupBool -> DataChecker Bool []
  _ ->  DataChecker String []
sup mode id loc ty1@String ty2@Bool = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@String ty2@Void = createIncompatible id loc ty1 ty2
--Bool
sup mode id loc ty1@Bool ty2@Int = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@Bool ty2@Real = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@Bool ty2@Char = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@Bool ty2@String = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
sup mode id loc ty1@Bool ty2@Bool = case mode of
  SupMod ->  createIncompatible id loc ty1 ty2
  SupPlus -> createIncompatible id loc ty1 ty2
  SupArith -> createIncompatible id loc ty1 ty2
  _ -> DataChecker Bool []
--Error
sup mode id loc Error Error = DataChecker Real []
sup mode id loc Error _ =  DataChecker Error []
sup mode id loc ty Error =  case mode of  
  SupDecl -> DataChecker ty []
  _ -> DataChecker Error []
--Array
sup mode id loc ar1@(Array typesFirst dimensionFirst) ar2@(Array typesSecond _) = 
  let DataChecker types errors = sup mode id  loc typesFirst typesSecond;
      errorConverted = map (errorIncompatibleTypesChange ar1 ar2) errors in 
  case types of 
    Error -> DataChecker Error errorConverted
    _ -> DataChecker (Array types dimensionFirst) errorConverted
sup mode id loc ty1@(Array _ _) ty2 = case mode of
  SupDecl -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _ -> createIncompatible id loc ty1 ty2
sup mode id loc ty1 ty2@(Array _ _) = case mode of 
  SupDecl -> createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  _ -> createIncompatible id loc ty1 ty2

createIncompatible id loc ty1 ty2 = DataChecker ty1 [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]

createIncompatibleRet id loc ty1 ty2 = DataChecker ty1 [ErrorChecker loc $ ErrorIncompatibleRetTypes id ty1 ty2]


errorIncompatibleTypesChange ty1 ty2 (ErrorChecker loc (ErrorIncompatibleDeclTypes id array types)) = 
  ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2
errorIncompatibleTypesChange ty1 ty2 error = error


errorConvertOVerloadingReturn locStart locEnd (ErrorChecker loc (ErrorIncompatibleDeclTypes id ty1 ty2)) =
  ErrorChecker loc $ ErrorOverloadingIncompatibleReturnType locStart locEnd id ty1 ty2


convertTypeSpecToTypeInferred Tint {} = Int
convertTypeSpecToTypeInferred Treal {} = Real
convertTypeSpecToTypeInferred Tchar {} = Char
convertTypeSpecToTypeInferred Tstring {} = String
convertTypeSpecToTypeInferred Tbool {} = Bool
convertTypeSpecToTypeInferred (TPointer _ ty) = Pointer $ convertTypeSpecToTypeInferred ty

convertMode PRef {} = Ref