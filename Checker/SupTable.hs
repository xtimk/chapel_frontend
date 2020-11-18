module Checker.SupTable where

import Checker.ErrorPrettyPrinter
import AbsChapel
import Utils.Type

data SupMode = SupDecl | SupFun | SupBool | SupPlus | SupMinus |  SupArith | SupMod | Sup | SupRet

supTac type1 type2 = 
  let (DataChecker tye _e) = sup Sup "id" (-3,-3) type1 type2 in
    tye
-- supTac Int Int = Int
-- supTac Int Real = Real
-- supTac Real Int = Real
-- supTac Char Int = Int
-- supTac Int Char = Int
-- supTac String String = String
-- supTac Char Char = Char
-- supTac Real Real = Real

-- supTac Char Real = Real
-- supTac Real Char = Real

-- supTac Bool Bool = Bool
-- supTac (Reference ty1) ty2 = supTac ty1 ty2
-- supTac ty1 (Reference ty2) = supTac ty1 ty2
-- supTac (Pointer ty1) ty2 = supTac ty1 ty2
-- supTac ty1 (Pointer ty2) = supTac ty1 ty2

-- supTac (Array ty bounds) Int = Int


-- supTac (Array Int bounds) (Array Real bounds2) = (Array Real bounds2)
-- supTac (Array Real bounds) (Array Int bounds2) = (Array Int bounds2)
-- supTac (Array Int bounds) (Array Int bounds2) = (Array Int bounds2)
-- supTac (Array Real bounds) (Array Real bounds2) = (Array Real bounds2)

-- supTac (Array complex1 bounds) (Array complex2 bounds2) = (Array Int bounds2)


--Void 
sup _ _ _ Void Void =  DataChecker Void []
sup _ _ loc Void ty2 = DataChecker ty2 [ErrorChecker loc ErrorReturnNotVoid]
sup _ _ loc ty1 Void = DataChecker ty1 [ErrorChecker loc ErrorFunctionVoid]
--Infered
sup _ _ _ Infered ty = DataChecker ty []
sup _ _ _ ty Infered = DataChecker ty []
--Reference
sup mode id loc (Reference ty1Ref) (Reference ty2Ref) = sup mode id loc ty1Ref ty2Ref
sup mode id loc ty1@(Reference ty1Ref) ty2 = case mode of 
  SupFun -> createIncompatible id loc ty1 ty2
  _ -> sup mode id loc ty1Ref ty2
sup mode id loc ty1 ty2@(Reference ty2Ref) = case mode of 
  SupFun -> createIncompatible id loc ty1 ty2
  _ -> sup mode id loc ty1 ty2Ref
--Pointer
sup mode id loc point@(Pointer _p1) (Pointer _p2) = let DataChecker ty errors = sup mode id loc _p1 _p2 in case mode of
  SupPlus -> DataChecker (Pointer ty) errors
  SupMinus -> DataChecker (Pointer ty) errors
  SupRet -> DataChecker (Pointer ty) errors
  SupDecl -> DataChecker (Pointer ty) errors
  _ -> DataChecker point [ErrorChecker loc ErrorWrongOperationAddress]
sup mode _ loc point@(Pointer _p) Int = case mode of 
  SupPlus -> DataChecker (Pointer _p) []
  SupMinus -> DataChecker (Pointer _p) []
  SupRet -> DataChecker (Pointer _p) []
  SupDecl -> DataChecker (Pointer _p) []
  _ -> DataChecker point [ErrorChecker loc $ ErrorCantOpToAddress Int]
sup _ _ loc (Pointer _p) ty = DataChecker _p [ErrorChecker loc $ ErrorCantOpToAddress ty]
sup mode _ loc Int point@(Pointer _p) = case mode of 
  SupPlus -> DataChecker (Pointer _p) []
  SupMinus -> DataChecker (Pointer _p) []
  _ -> DataChecker point [ErrorChecker loc $ ErrorCantOpToAddress Int]
sup _ _ loc ty (Pointer _p) = DataChecker _p [ErrorChecker loc $ ErrorCantOpToAddress ty]
--Int 
sup mode _ _ Int Int = case mode of 
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
sup _ id loc ty1@Int ty2@String = createIncompatible id loc ty1 ty2
sup _ id loc ty1@Int ty2@Bool = createIncompatible id loc ty1 ty2
--Real
sup mode id loc ty1@Real ty2@Int = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupMod ->  createIncompatible id loc ty1 ty2
  SupRet -> createIncompatibleRet id loc ty1 ty2
  SupBool -> DataChecker Bool []
  _ -> DataChecker Real []
sup mode id loc ty1@Real ty2@Real = case mode of
  SupMod ->  createIncompatible id loc ty1 ty2 
  SupBool -> DataChecker Bool []
  _ -> DataChecker Real []
sup mode id loc ty1@Real ty2@Char = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupMod ->  createIncompatible id loc ty1 ty2
  SupBool -> DataChecker Bool []
  _ -> DataChecker Real []
sup _ id loc ty1@Real ty2@String = createIncompatible id loc ty1 ty2
sup mode id loc ty1@Real ty2@Bool = case mode of
  SupRet -> createIncompatibleRet id loc ty1 ty2
  SupFun -> createIncompatible id loc ty1 ty2
  _otherwhise -> createIncompatible id loc ty1 ty2
--Char
sup mode id loc ty1@Char ty2@Int = case mode of
  SupFun -> createIncompatible id loc ty1 ty2
  SupBool -> DataChecker Bool []
  _ -> DataChecker Char []
sup _ id loc ty1@Char ty2@Real =  createIncompatible id loc ty1 ty2
sup mode _ _ Char Char = case mode of 
  SupBool -> DataChecker Bool []
  _ -> DataChecker Char []
sup _ id loc ty1@Char ty2@String = createIncompatible id loc ty1 ty2
sup _ id loc ty1@Char ty2@Bool = createIncompatible id loc ty1 ty2
--String
sup _ id loc ty1@String ty2@Int = createIncompatible id loc ty1 ty2
sup _ id loc ty1@String ty2@Real =createIncompatible id loc ty1 ty2
sup _ id loc ty1@String ty2@Char = createIncompatibleRet id loc ty1 ty2
sup mode id loc ty1@String ty2@String = case mode of
  SupFun ->  DataChecker String []
  SupRet ->  DataChecker String []
  SupDecl ->  DataChecker String []
  _ ->  createIncompatible id loc ty1 ty2
sup _ id loc ty1@String ty2@Bool = createIncompatible id loc ty1 ty2
--Bool
sup _ id loc ty1@Bool ty2@Int = createIncompatible id loc ty1 ty2
sup _ id loc ty1@Bool ty2@Real = createIncompatible id loc ty1 ty2
sup _ id loc ty1@Bool ty2@Char =  createIncompatible id loc ty1 ty2
sup _ id loc ty1@Bool ty2@String = createIncompatible id loc ty1 ty2
sup mode id loc ty1@Bool ty2@Bool = case mode of
  SupMod ->  createIncompatible id loc ty1 ty2
  SupPlus -> createIncompatible id loc ty1 ty2
  SupArith -> createIncompatible id loc ty1 ty2
  _ -> DataChecker Bool []
--Error
sup _ _ _ Error Error = DataChecker Real []
sup _ _ _ Error _ =  DataChecker Error []
sup mode _ _ ty Error =  case mode of  
  SupDecl -> DataChecker ty []
  _ -> DataChecker Error []
--Array
sup mode id loc ar1@(Array typesFirst firstDim) ar2@(Array typesSecond _) = 
  let lenghtFirst = getArrayLenght ar1
      lenghSecond = getArrayLenght ar2 in 
  if lenghtFirst ==  lenghSecond
  then 
    let DataChecker types errors = sup mode id loc typesFirst typesSecond;
        errorConverted = map (errorIncompatibleTypesChange ar1 ar2) errors in 
    case types of 
      Error -> DataChecker Error errorConverted
      _ -> DataChecker (Array types firstDim) errorConverted
  else DataChecker Error [ErrorChecker loc $ IncompatibleArrayDimension lenghtFirst lenghSecond]
sup _ id loc ty1@(Array _ _) ty2 =  createIncompatible id loc ty1 ty2
sup _ id loc ty1 ty2@(Array _ _) = createIncompatible id loc ty1 ty2

createIncompatible id loc ty1 ty2 = DataChecker ty1 [ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2]

createIncompatibleRet id loc ty1 ty2 = DataChecker ty1 [ErrorChecker loc $ ErrorIncompatibleRetTypes id ty1 ty2]


errorIncompatibleTypesChange ty1 ty2 (ErrorChecker loc (ErrorIncompatibleDeclTypes id _ _)) = 
  ErrorChecker loc $ ErrorIncompatibleDeclTypes id ty1 ty2
errorIncompatibleTypesChange _ _ error = error


errorConvertOVerloadingReturn locStart locEnd (ErrorChecker loc (ErrorIncompatibleDeclTypes id ty1 ty2)) =
  ErrorChecker loc $ ErrorOverloadingIncompatibleReturnType locStart locEnd id ty1 ty2


convertTypeSpecToTypeInferred Tint {} = Int
convertTypeSpecToTypeInferred Treal {} = Real
convertTypeSpecToTypeInferred Tchar {} = Char
convertTypeSpecToTypeInferred Tstring {} = String
convertTypeSpecToTypeInferred Tbool {} = Bool
convertTypeSpecToTypeInferred (TPointer _ ty) = Pointer $ convertTypeSpecToTypeInferred ty

convertMode (RefMode (PRef _) ) = Utils.Type.Ref

convertTyMode mode ty = case mode of
  Ref -> Reference ty
  _ -> ty