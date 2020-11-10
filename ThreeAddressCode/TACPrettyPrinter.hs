module ThreeAddressCode.TACPrettyPrinter where
import ThreeAddressCode.TAC

printTacEntries ::(Foldable t0) => t0 TACEntry -> IO ()
printTacEntries = mapM_ printTacEntry

printTacEntry (TACEntry label operation) = putStrLn $ printLabel label ++ printTacEntry' operation

printTacEntry' operation = case operation of
    Binary temp1 temp2 bop temp3 -> printTacTemp temp1 ++ " = " ++ printTacTemp temp2 ++  printTacBop bop ++ printTacTemp temp3
    Unary temp1 uop  temp2 -> printTacTemp temp1 ++ " = " ++ printTacUop uop ++ printTacTemp temp2
    Nullary temp1 temp2 ->  printTacTemp temp1 ++ " = " ++ printTacTemp temp2
    UnconJump label -> "goto " ++ printLabelGoto label
    BoolTrueCondJump temp label -> "if " ++ printTacTemp temp ++ " goto " ++ printLabelGoto label
    BoolFalseCondJump temp label -> "ifFalse " ++ printTacTemp temp ++ " goto " ++ printLabelGoto label
    RelCondJump temp1 rel temp2 label -> "if " ++ printTacTemp temp1 ++ printTacRel rel ++ printTacTemp temp2 ++ " goto " ++ printLabelGoto label
    IndexLeft temp1 temp2 temp3 ->  printTacTemp temp1 ++ "[" ++ printTacTemp temp2 ++ "]" ++ " = " ++ printTacTemp temp3
    IndexRight temp1 temp2 temp3 -> printTacTemp temp1 ++ " = " ++  printTacTemp temp2 ++  "[" ++ printTacTemp temp3 ++ "]"
    DeferenceRight temp1 temp2 -> printTacTemp temp1 ++ " = &" ++  printTacTemp temp2
    ReferenceLeft temp1 temp2 -> "*" ++ printTacTemp temp1 ++ " = " ++  printTacTemp temp2
    ReferenceRight temp1 temp2 -> printTacTemp temp1 ++ " = *" ++  printTacTemp temp2
    SetParam temp -> "param " ++ printTacTemp temp 
    CallProc temp1 arity -> "call " ++ printTacTemp temp1 ++ ", " ++ show arity 
    CallFun temp1 temp2 arity -> printTacTemp temp1 ++ " = fcall " ++ printTacTemp temp2 ++ ", " ++ show arity 
    ReturnVoid -> "return"
    ReturnValue temp -> "return " ++ printTacTemp temp
    VoidOp -> ""


printLabel (Just ("FALL",_)) = "         "
printLabel (Just (lab,(l,c))) = lab ++ "@" ++ show l ++ "," ++ show c ++ ": "
printLabel Nothing = "         "

printLabelGoto (lab,(l,c)) = lab ++ "@" ++ show l ++ "," ++ show c 

printTacTemp (Temp mode id (l,c) _) = case mode of
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
    
printTacRel rel = case rel of
    ThreeAddressCode.TAC.LT -> " < "
    ThreeAddressCode.TAC.GT -> " > "
    ThreeAddressCode.TAC.LTE -> " <= "
    ThreeAddressCode.TAC.GTE -> " >= "
    ThreeAddressCode.TAC.EQ -> " = "
    ThreeAddressCode.TAC.NEQ -> " != "