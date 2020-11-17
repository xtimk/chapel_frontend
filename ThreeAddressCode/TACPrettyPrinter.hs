module ThreeAddressCode.TACPrettyPrinter where
import ThreeAddressCode.TAC
import Utils.Type
import Data.Char

printTacEntries ::(Foldable t0) => t0 TACEntry -> IO ()
printTacEntries = mapM_ printTacEntry

printTacEntry (TACEntry label operation) = putStrLn $ printLabel label ++ printTacEntry' operation

printTacEntry' operation = case operation of
    Binary temp1 temp2 bop temp3 -> 
        let tye1 = getTacTempTye temp2
            tye2 = getTacTempTye temp3 in
        case tye1 == tye2 of
        True -> printTacTemp temp1 ++ " = " ++ printTacTemp temp2 ++  printTacBop bop tye1 ++ printTacTemp temp3
        False -> 
            let suptype = tacsup tye1 tye2 in
            printTacTemp temp1 ++ " = " ++ auxPrintTacTemp temp2 suptype ++  printTacBop bop suptype ++ auxPrintTacTemp temp3 suptype
    Unary temp1 uop  temp2 -> printTacTemp temp1 ++ " = " ++ printTacUop uop ++ printTacTemp temp2
    Nullary temp1 temp2 ->  printTacTemp temp1 ++ " = " ++ printTacTemp temp2
    UnconJump label -> "goto " ++ printLabelGoto label
    BoolTrueCondJump temp label -> "if " ++ printTacTemp temp ++ " goto " ++ printLabelGoto label
    BoolFalseCondJump temp label -> "ifFalse " ++ printTacTemp temp ++ " goto " ++ printLabelGoto label
    RelCondJump temp1 rel temp2 label -> 
        let tye1 = getTacTempTye temp1
            tye2 = getTacTempTye temp2 in
        case tye1 == tye2 of
            True -> "if " ++ printTacTemp temp1 ++ printTacRel rel tye1 ++ printTacTemp temp2 ++ " goto " ++ printLabelGoto label
            False -> let suptype = tacsup tye1 tye2 in
                "if " ++ auxPrintTacTemp temp1 suptype ++ printTacRel rel suptype ++ auxPrintTacTemp temp2 suptype ++ " goto " ++ printLabelGoto label
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


auxPrintTacTemp temp tye = 
    case getTacTempTye temp == tye of
        True -> printTacTemp temp
        False -> (printCast (getTacTempTye temp) tye) ++ "(" ++ printTacTemp temp ++ ")"
tacsup Int Real = Real
tacsup Real Int = Real

printCast Int Real = "cast_int_to_real"

printLabel (Just ("FALL",_, _)) = "         "
printLabel (Just (lab,(l,c),ty)) = lab ++ printLabelType ty ++ "@" ++ show l ++ "," ++ show c ++ ": "
printLabel Nothing = "         "

printLabelType ty = case ty of
    TrueBoolStmLb -> "true"
    FalseBoolStmLb -> "false"
    ExitBoolStmLb -> "exit"
    _ -> ""

printLabelGoto (lab,(l,c),ty) = lab ++ printLabelType ty ++ "@" ++ show l ++ "," ++ show c 

printTacTemp (Temp mode id (l,c) _) = case mode of
    Fixed -> id
    _ -> id ++ "@" ++ show l ++ "," ++ show c

getTacTempTye (Temp _ _ _ tye) = tye

printTacBop bop ty = case bop of
    Plus -> " plus_" ++ (map toLower (show ty)) ++ " "
    Minus -> " minus_" ++ (map toLower (show ty)) ++ " "
    Times -> " mul_" ++ (map toLower (show ty)) ++ " "
    Div -> " div_" ++ (map toLower (show ty)) ++ " "
    Modul -> " mod_" ++ (map toLower (show ty)) ++ " "

printTacUop uop = case uop of
    Neg -> " not "
    
printTacRel rel ty = case rel of
    ThreeAddressCode.TAC.LT -> " lt_" ++ (map toLower (show ty)) ++ " "
    ThreeAddressCode.TAC.GT -> " gt_" ++ (map toLower (show ty)) ++ " "
    ThreeAddressCode.TAC.LTE -> " lte_" ++ (map toLower (show ty)) ++ " "
    ThreeAddressCode.TAC.GTE -> " gte_" ++ (map toLower (show ty)) ++ " "
    ThreeAddressCode.TAC.EQ -> " eq_" ++ (map toLower (show ty)) ++ " "
    ThreeAddressCode.TAC.NEQ -> " neq_" ++ (map toLower (show ty)) ++ " "