module ThreeAddressCode.TACPrettyPrinter where
import ThreeAddressCode.TAC

printTacEntries ::(Foldable t0) => t0 TACEntry -> IO ()
printTacEntries = mapM_ printTacEntry

printTacEntry entry = putStrLn $ printTacEntry' entry

printTacEntry' (TACEntry label operation) = case operation of
    Binary temp1 temp2 bop temp3 -> printTacTemp temp1 ++ " = " ++ printTacTemp temp2 ++  printTacBop bop ++ printTacTemp temp3
    Unary temp1 uop  temp2 -> printTacTemp temp1 ++ " = " ++ printTacUop uop ++ printTacTemp temp2
    Nullary temp1 temp2 ->  printTacTemp temp1 ++ " = " ++ printTacTemp temp2
    UnconJump label -> ""
    BoolCondJump temp label -> ""
    RelCondJump temp1 rel temp2 temp3 -> ""
    IndexLeft temp1 temp2 temp3 ->  printTacTemp temp1 ++ "[" ++ printTacTemp temp2 ++ "]" ++ " = " ++ printTacTemp temp3
    IndexRight temp1 temp2 temp3 -> printTacTemp temp1 ++ " = " ++  printTacTemp temp2 ++  "[" ++ printTacTemp temp3 ++ "]"
    DeferenceRight temp1 temp2 -> ""
    ReferenceLeft temp1 temp2 -> ""
    RefereceneRight temp1 temp2 -> ""
    SetParam temp -> ""
    CallProc temp1 temp2 -> ""
    CallFun temp1 temp2 temp3 -> ""
    ReturnVoid -> ""
    ReturnValue temp -> ""

printTacTemp (Temp mode id (l,c) ty) = case mode of
    Var -> id ++ "@" ++ show l ++ "," ++ show c
    Fix -> id

printTacBop bop = case bop of
    Plus -> " + "
    Minus -> " - "
    Times -> " * "
    Div -> " / "
    Modul -> " % "

printTacUop uop = case uop of
    Neg -> " not "
    
--printTacRel rel = case rel of
   -- LT -> " < "
   -- GT -> " > "
   -- LTE -> " >= "
   -- GTE -> " <= "
   -- EQ -> " = "
    --NEQ -> " != "