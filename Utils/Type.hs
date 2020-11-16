module Utils.Type where 

data Mode = Normal | Ref | Out | Name | In 
 deriving (Show)

data Type = Int | Real | Bool | Void | Char | String | Infered | Array Type (Bound , Bound) | Pointer Type | Reference Type | Error
 deriving (Show)

data Bound = Var String | Fix Int
  deriving (Show)

instance Eq Type where
    x == y = case (x,y) of
      (Array ty1 _, Array ty2 _) -> ty1 == ty2
      (_,_) -> show x == show y

getSubarrayDimension types 0 = types
getSubarrayDimension (Array subtype _) i = getSubarrayDimension subtype (i - 1)

getArrayDimension (Array subtype _) = 1 + getArrayDimension subtype
getArrayDimension _ = 0  

getArrayLenght (Array _ (boundLeft, boundRight)) = case (boundLeft,boundRight) of
  (Fix a, Fix b) -> b - a + 1
  _ -> 0

getBasicType ty = case ty of
  Array subType _ -> getBasicType subType
  Reference subType -> getBasicType subType
  Pointer subType -> getBasicType subType
  subType -> subType
   