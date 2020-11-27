module Checker.SupTable where

import Utils.Type

data SupMode = SupDecl | SupFun | SupBool | SupPlus | SupMinus | SupArith | SupMod | Sup | SupRet
  deriving Show

instance Eq SupMode where
  x == y = show x == show y

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
--Pointer
sup mode point@(Pointer _p1) (Pointer _p2) = 
  let (ty,compatibility) = sup mode _p1 _p2 in 
  pointerAux mode ty compatibility
    where 
      pointerAux mode ty compatibility
        | mode `elem`[SupPlus,SupMinus,SupRet,SupFun,Sup,SupDecl] = (Pointer ty,compatibility)
        | otherwise = incompatible point
sup mode point@(Pointer _p) Int
  | mode `elem` [SupPlus, SupMinus, SupRet,SupDecl] = compatible (Pointer _p)
  | otherwise = incompatible point
sup _ point@(Pointer _p) _ = incompatible point 
sup mode Int (Pointer _p) 
  | mode `elem` [SupPlus,SupMinus] = compatible (Pointer _p)
  | otherwise = incompatible Int
sup _ ty (Pointer _p) = incompatible ty  
--Int 
sup mode Int Int
  | mode == SupBool = compatible Bool
  | otherwise = compatible Int
sup mode ty1@Int Real
 | mode `elem` [SupMod, SupDecl, SupRet, SupFun] = incompatible ty1
 | mode == SupBool = compatible Bool
 | otherwise = compatible Real
sup mode ty1@Int Char
  | mode == SupFun = incompatible ty1
  | otherwise = compatible Int
sup _ ty1@Int String = incompatible ty1
sup _ ty1@Int Bool = incompatible ty1
--Real
sup mode ty1@Real Int
  | mode `elem` [SupFun,SupMod] = incompatible ty1
  | mode == SupBool = compatible Bool
  | otherwise = compatible Real
sup mode ty1@Real Real
  | mode == SupMod = incompatible ty1
  | mode == SupBool = compatible Bool
  | otherwise = compatible Real
sup mode ty1@Real Char
  | mode `elem` [SupFun,SupMod] = incompatible ty1
  | mode == SupBool = compatible Bool
  | otherwise = compatible Real
sup _ ty1@Real String = incompatible ty1
sup _ ty1@Real Bool = incompatible ty1
--Char
sup mode ty1@Char Int
  | mode `elem` [SupFun, SupDecl, SupRet] = incompatible ty1
  | mode == SupBool = compatible Bool
  | otherwise = compatible Int
sup mode ty1@Char Real
  | mode `elem` [SupFun, SupDecl, SupRet] = incompatible ty1
  | mode == SupBool = compatible Bool
  | otherwise = compatible Real
sup mode Char Char
  | mode == SupBool = compatible Bool
  | mode `elem` [SupMinus, SupArith, SupMod, SupPlus, Sup] = compatible Int 
  | otherwise = compatible Char
sup _ ty1@Char String = incompatible ty1
sup _ ty1@Char Bool = incompatible ty1
--String
sup _ ty1@String Int = incompatible ty1
sup _ ty1@String Real = incompatible ty1
sup _ ty1@String Char = incompatible ty1 
sup mode ty1@String String 
 | mode `elem` [SupFun, SupRet, SupDecl, Sup] = compatible String
 | mode == SupBool = compatible Bool
 | otherwise = incompatible ty1 
sup _ ty1@String Bool = incompatible ty1 
--Bool
sup _ ty1@Bool Int = incompatible ty1 
sup _ ty1@Bool Real = incompatible ty1 
sup _ ty1@Bool Char =  incompatible ty1 
sup _ ty1@Bool String = incompatible ty1 
sup mode ty1@Bool Bool 
  | mode `elem` [SupMod, SupPlus, SupArith] = incompatible ty1
  | otherwise = compatible Bool
--Error
sup _ Error Error = compatible Error
sup _ Error _ =  compatible Error
sup mode ty Error =  case mode of  
  SupDecl -> compatible ty 
  _ -> compatible Error
--Array
sup mode ar1@(Array typesFirst firstDim) ar2@(Array typesSecond _)
  | mode `elem` [SupDecl, SupRet, SupFun, Sup] = 
      let lenghtFirst = getArrayLenght ar1
          lenghSecond = getArrayLenght ar2 in 
      if lenghtFirst ==  lenghSecond
      then 
        let (types, compatibility) = sup mode typesFirst typesSecond in 
        case types of 
          Error -> (Error,compatibility)
          _ -> (Array types firstDim, compatibility)
      else incompatible Error
  | otherwise = incompatible Error
sup _ ty1@(Array _ _) _ =  incompatible ty1
sup _ ty1 (Array _ _) = incompatible ty1

compatible ty = (ty,True)
incompatible ty = (ty,False)






