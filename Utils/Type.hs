module Utils.Type where 

data Mode = Normal | Ref | Out | Name | In 
 deriving (Show)

data Type = Int | Real | Bool | Void | Char | String | Infered | Array Type (Bound , Bound) | Pointer Type | Reference Type | Error
 deriving (Show)

type Bound = Int

instance Eq Type where
    x == y = case (x,y) of
      (Array ty1 _, Array ty2 _) -> ty1 == ty2
      (_,_) -> show x == show y


      
convertTyMode mode ty = case mode of
  Ref -> Reference ty
  _ -> ty

getSubarrayDimension types 0 = types
getSubarrayDimension (Array subtype _) i = getSubarrayDimension subtype (i - 1)

getArrayDimension (Array subtype _) = 1 + getArrayDimension subtype
getArrayDimension _ = 0  

getArrayLenght (Array _ (boundLeft, boundRight)) = boundRight - boundLeft + 1

getArrayOffset (Array _ (boundLeft, _)) = boundLeft


getNoModeType ty = case ty of
  Reference ty -> ty
  _ -> ty

getBasicType ty = case ty of
  Array subType _ -> getBasicType subType
  Reference subType -> getBasicType subType
  Pointer subType -> getBasicType subType
  subType -> subType

getBasicTacType ty = case ty of
  Array subType _ -> getBasicType subType
  Reference subType -> getBasicType subType
  subType -> subType

getArrayPrimaryType (Array subType _) = subType
getArrayPrimaryType g = g
   