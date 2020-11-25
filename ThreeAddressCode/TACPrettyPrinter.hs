module ThreeAddressCode.TACPrettyPrinter where
import ThreeAddressCode.TAC
import Utils.Type
import Data.Char

--printTacEntries ::(Foldable t0) => t0 TACEntry -> IO ()
printTacEntries m = mapM_ (printTacEntry m)

findMaxLenOfLabels xs = maximum (map getLabelLenFromTACEntry xs)

getLabelLenFromTACEntry (TACEntry label _ _) = case label of
    Nothing -> 0
    Just (lab,pos,ty) -> length lab + length (show pos) + getLabelTypeLen ty

printTacEntry m (TACEntry label operation comment) = case operation of
    CommentOp _-> putStrLn $ printTacEntry' operation ++ printTacComment comment
    _ -> putStrLn $ printLabel m label ++ printTacEntry' operation ++ printTacComment comment

printTacComment Nothing = ""
printTacComment (Just c) = ' ':'#':' ':c

printTacEq tye = " =" ++ printTacEqAux tye ++ " "

printTacEqAux tye = 
    case tye of
        (Pointer _) -> "_addr"
        (Array _ _) -> "_addr" --map toLower $ '_' : printTyAux  t
        Real -> "_float"
        Reference ty -> map toLower $ '_' : printTyAux ty
        _ -> map toLower $ '_' : printTyAux tye



printTacEntry' operation = case operation of
    Binary temp1 temp2 bop temp3 -> -- printTacTemp temp1 ++ " = " ++ printTacTemp temp2 ++  printTacBop bop tye1 ++ printTacTemp temp3
        let tye0 = getTacTempTye temp1
            tye1 = getTacTempTye temp2
            _tye2 = getTacTempTye temp3 in
        printTacTemp temp1 ++ printTacEq tye0 ++ printTacTemp temp2 ++  printTacBop bop tye1 ++ printTacTemp temp3
    Unary temp1 uop  temp2 -> 
        let tye0 = getTacTempTye temp1 in
        printTacTemp temp1 ++ printTacEq tye0 ++ printTacUop uop ++ printTacTemp temp2
    Nullary temp1 temp2 -> let tye0 = getTacTempTye temp1 in
        printTacTemp temp1 ++ printTacEq tye0 ++ printTacTemp temp2
    UnconJump label -> "goto " ++ printLabelGoto label
    BoolTrueCondJump temp label -> "if " ++ printTacTemp temp ++ " goto " ++ printLabelGoto label
    BoolFalseCondJump temp label -> "ifFalse " ++ printTacTemp temp ++ " goto " ++ printLabelGoto label
    RelCondJump temp1 rel temp2 label -> -- "if " ++ printTacTemp temp1 ++ printTacRel rel tye1 ++ printTacTemp temp2 ++ " goto " ++ printLabelGoto label
        let tye1 = getTacTempTye temp1
            _tye2 = getTacTempTye temp2 in
        "if " ++ printTacTemp temp1 ++ printTacRel rel tye1 ++ printTacTemp temp2 ++ " goto " ++ printLabelGoto label
    IndexLeft temp1 temp2 temp3 ->  printTacTemp temp1 ++ "[" ++ printTacTemp temp2 ++ "]" ++ printTacEq (getTacTempTye temp1) ++ printTacTemp temp3
    IndexRight temp1 temp2 temp3 -> printTacTemp temp1 ++ printTacEq (getTacTempTye temp1) ++  printTacTemp temp2 ++  "[" ++ printTacTemp temp3 ++ "]"
    DeferenceRight temp1 temp2 -> printTacTemp temp1 ++ printTacEq (getTacTempTye temp1) ++ "&" ++  printTacTemp temp2
    ReferenceLeft temp1 temp2 -> "*" ++ printTacTemp temp1 ++ printTacEq (getTacTempTye temp1) ++  printTacTemp temp2
    ReferenceRight temp1 temp2 -> printTacTemp temp1 ++ printTacEq (getTacTempTye temp1) ++ "*" ++  printTacTemp temp2
    SetParam temp -> "param " ++ printTacTemp temp 
    CallProc temp1 arity -> "call " ++ printTacTemp temp1 ++ ", " ++ show arity 
    CallFun temp1 temp2 arity -> printTacTemp temp1 ++ printTacEq (getTacTempTye temp1) ++ " fcall " ++ printTacTemp temp2 ++ ", " ++ show arity 
    ReturnVoid -> "return"
    ReturnValue temp -> "return " ++ printTacTemp temp
    VoidOp -> ""
    Cast temp1 CastIntToFloat temp2 -> printTacTemp temp1 ++ printTacEq (getTacTempTye temp1) ++ "cast_int_to_float " ++ printTacTemp temp2
    Cast temp1 CastCharToInt temp2 -> printTacTemp temp1 ++ printTacEq (getTacTempTye temp1) ++ "cast_char_to_int " ++ printTacTemp temp2
    Cast temp1 CastIntToChar temp2 -> printTacTemp temp1 ++ printTacEq (getTacTempTye temp1) ++ "cast_int_to_char " ++ printTacTemp temp2
    Cast temp1 CastCharToFloat temp2 -> printTacTemp temp1 ++ printTacEq (getTacTempTye temp1) ++ "cast_char_to_float " ++ printTacTemp temp2
    StringOp id -> id
    CommentOp id -> "## " ++ id


printLabel m (Just ("FALL",_, _)) = replicate (m+4) ' '
printLabel m (Just (lab,pos@(l,c),ty)) = lab ++ printLabelType ty ++ "@" ++ show l ++ "," ++ show c ++ ":    " ++ replicate (m - (length lab + length (show pos) + getLabelTypeLen ty )) ' '
printLabel m Nothing = replicate (m+4) ' '

printLabelType ty = case ty of
    TrueBoolStmLb -> "true"
    FalseBoolStmLb -> "false"
    ExitBoolStmLb -> "exit"
    _ -> ""

getLabelTypeLen ty = case ty of
    TrueBoolStmLb -> 4
    FalseBoolStmLb -> 5
    ExitBoolStmLb -> 4
    _ -> 0

printLabelGoto (lab,(l,c),ty) = lab ++ printLabelType ty ++ "@" ++ show l ++ "," ++ show c 

printTacTemp (Temp mode id (l,c) _) = case mode of
    Fixed -> id
    Temporary -> id ++ "@@" ++ show l ++ "," ++ show c
    _ -> id ++ "@" ++ show l ++ "," ++ show c

printTacBop bop ty = 
    case ty of
    (Pointer _) -> case bop of
            Plus -> " plus_int "
            Minus -> " minus_int "
    (Array t _) -> case bop of
        Plus -> " plus_" ++ map toLower (printTyAux (getBasicTacType t)) ++ " "
        Minus -> " minus_" ++ map toLower (printTyAux (getBasicTacType t)) ++ " "
        Times -> " mul_" ++ map toLower (printTyAux (getBasicTacType t)) ++ " "
        Div -> " div_" ++ map toLower (printTyAux (getBasicTacType t)) ++ " "
        Modul -> " mod_" ++ map toLower (printTyAux (getBasicTacType t)) ++ " "
        Pow -> " pow_" ++ map toLower (printTyAux ty) ++ " "

    _ -> case bop of
            Plus -> " plus_" ++ map toLower (printTyAux ty) ++ " "
            Minus -> " minus_" ++ map toLower (printTyAux ty) ++ " "
            Times -> " mul_" ++ map toLower (printTyAux ty) ++ " "
            Div -> " div_" ++ map toLower (printTyAux ty) ++ " "
            Modul -> " mod_" ++ map toLower (printTyAux ty) ++ " "
            Pow -> " pow_" ++ map toLower (printTyAux ty) ++ " "

-- la uso per stampare float invece che real nel TAC
printTyAux ty = case ty of
    Real -> "float"
    Pointer ty -> "addr"
    _ -> show ty

getTacTempTye (Temp _ _ _ tye) = tye

printTacUop uop = case uop of
    Neg -> " not "
    MinusUnaryOp -> " -"
    
printTacRel rel ty = 
    case ty of
    (Pointer p) -> 
        case rel of
            ThreeAddressCode.TAC.LT -> " lt_int "
            ThreeAddressCode.TAC.GT -> " gt_int "
            ThreeAddressCode.TAC.LTE -> " lte_int "
            ThreeAddressCode.TAC.GTE -> " gte_int "
            ThreeAddressCode.TAC.EQ -> " eq_int "
            ThreeAddressCode.TAC.NEQ -> " neq_int "
    _ ->
        case rel of
            ThreeAddressCode.TAC.LT -> " lt_" ++ map toLower (printTyAux ty) ++ " "
            ThreeAddressCode.TAC.GT -> " gt_" ++ map toLower (printTyAux ty) ++ " "
            ThreeAddressCode.TAC.LTE -> " lte_" ++ map toLower (printTyAux ty) ++ " "
            ThreeAddressCode.TAC.GTE -> " gte_" ++ map toLower (printTyAux ty) ++ " "
            ThreeAddressCode.TAC.EQ -> " eq_" ++ map toLower (printTyAux ty) ++ " "
            ThreeAddressCode.TAC.NEQ -> " neq_" ++ map toLower (printTyAux ty) ++ " "