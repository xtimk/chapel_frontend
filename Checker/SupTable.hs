module Checker.SupTable where

import AbsChapel
import Utils.Type

data SupMode = SupDecl | SupFun | SupBool | SupPlus | SupMinus |  SupArith | SupMod | Sup | SupRet

supTac supmode type1 type2 = 
  let (tye, _) = sup supmode type1 type2 in
    tye

--Void 
sup _ Void Void =  compatible Void
sup _ ty1@Void _ = incompatible ty1
sup _ ty1 Void = incompatible ty1
--Infered
sup _ Infered ty = compatible ty
sup _ ty Infered = compatible ty
--Reference
sup mode (Reference ty1Ref) (Reference ty2Ref) = sup mode ty1Ref ty2Ref
sup mode ty1@(Reference ty1Ref) ty2 = case mode of 
  SupFun -> incompatible ty1
  _ -> sup mode ty1Ref ty2
sup mode ty1 ty2@(Reference ty2Ref) = case mode of 
  SupFun -> incompatible ty1
  _ -> sup mode ty1 ty2Ref
--Pointer
sup mode point@(Pointer _p1) (Pointer _p2) = let (ty,compatibility) = sup mode _p1 _p2 in case mode of
  SupPlus -> (Pointer ty,compatibility)
  SupMinus -> (Pointer ty,compatibility)
  SupRet -> (Pointer ty,compatibility)
  SupFun -> (Pointer ty,compatibility)
  Sup -> (Pointer ty,compatibility)
  SupDecl -> (Pointer ty,compatibility)
  _ -> incompatible point
sup mode point@(Pointer _p) Int = case mode of 
  SupPlus -> compatible (Pointer _p)
  SupMinus -> compatible (Pointer _p)
  SupRet -> compatible (Pointer _p)
  SupDecl -> compatible (Pointer _p)
  _ -> incompatible point
sup _ point@(Pointer _p) _ = incompatible point 
sup mode Int (Pointer _p) = case mode of 
  SupPlus -> compatible (Pointer _p)
  SupMinus -> compatible (Pointer _p)
  _ -> incompatible Int
sup _ ty (Pointer _p) = incompatible ty  
--Int 
sup mode Int Int = case mode of 
  SupBool -> compatible Bool
  _ -> compatible Int
sup mode ty1@Int Real = case mode of
  SupMod ->  incompatible ty1
  SupDecl -> incompatible ty1
  SupRet -> incompatible ty1
  SupFun -> incompatible ty1
  SupBool -> compatible Bool
  _ -> compatible Real
sup mode ty1@Int Char = case mode of 
  SupFun -> incompatible ty1
  _ -> compatible Int
sup _ ty1@Int String = incompatible ty1
sup _ ty1@Int Bool = incompatible ty1
--Real
sup mode ty1@Real Int = case mode of
  SupFun -> incompatible ty1
  SupMod ->  incompatible ty1
  SupBool -> compatible Bool
  _ -> compatible Real
sup mode ty1@Real Real = case mode of
  SupMod ->  incompatible ty1
  SupBool -> compatible Bool
  _ -> compatible Real
sup mode ty1@Real Char = case mode of
  SupFun -> incompatible ty1
  SupMod ->  incompatible ty1
  SupBool -> compatible Bool
  _ -> compatible Real
sup _ ty1@Real String = incompatible ty1
sup _ ty1@Real Bool = incompatible ty1
--Char
sup mode ty1@Char ty2@Int = case mode of
  SupFun -> incompatible ty2
  SupDecl -> incompatible ty1
  SupRet -> incompatible ty1
  SupBool -> compatible Bool
  _ -> compatible Int
sup mode ty1@Char ty2@Real = case mode of
  SupFun -> incompatible ty2
  SupDecl -> incompatible ty1
  SupRet -> incompatible ty1
  SupBool -> compatible Bool
  _ -> compatible Real
sup mode Char Char = case mode of 
  SupBool -> compatible Bool
  SupMinus -> compatible Int 
  SupArith -> compatible Int
  SupMod -> compatible Int 
  SupPlus -> compatible Int
  Sup -> compatible Int
  _ -> compatible Char
sup _ ty1@Char String = incompatible ty1
sup _ ty1@Char Bool = incompatible ty1
--String
sup _ ty1@String Int = incompatible ty1
sup _ ty1@String Real = incompatible ty1
sup _ ty1@String Char = incompatible ty1 
sup mode ty1@String String = case mode of
  SupFun ->  compatible String
  SupRet ->  compatible String
  SupDecl ->  compatible String
  Sup -> compatible String
  _ ->  incompatible ty1 
sup _ ty1@String Bool = incompatible ty1 
--Bool
sup _ ty1@Bool Int = incompatible ty1 
sup _ ty1@Bool Real = incompatible ty1 
sup _ ty1@Bool Char =  incompatible ty1 
sup _ ty1@Bool String = incompatible ty1 
sup mode ty1@Bool Bool = case mode of
  SupMod ->  incompatible ty1
  SupPlus -> incompatible ty1
  SupArith -> incompatible ty1
  _ -> compatible Bool
--Error
sup _ Error Error = compatible Error
sup _ Error _ =  compatible Error
sup mode ty Error =  case mode of  
  SupDecl -> compatible ty 
  _ -> compatible Error
--Array
sup mode ar1@(Array typesFirst firstDim) ar2@(Array typesSecond _) = 
  let lenghtFirst = getArrayLenght ar1
      lenghSecond = getArrayLenght ar2 in 
  if lenghtFirst ==  lenghSecond
  then 
    let (types, compatibility) = sup mode typesFirst typesSecond in 
    case types of 
      Error -> (Error,compatibility)
      _ -> (Array types firstDim, compatibility)
  else incompatible Error
sup _ ty1@(Array _ _) _ =  incompatible ty1
sup _ ty1 (Array _ _) = incompatible ty1

compatible ty = (ty,True)
incompatible ty = (ty,False)


convertMode (RefMode (PRef _) ) = Utils.Type.Ref

convertTyMode mode ty = case mode of
  Ref -> Reference ty
  _ -> ty