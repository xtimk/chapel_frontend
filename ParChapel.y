-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParChapel where
import AbsChapel
import LexChapel
import ErrM

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  ',' { PT _ (TS _ 1) }
  L_POpenGraph { PT _ (T_POpenGraph _) }
  L_PCloseGraph { PT _ (T_PCloseGraph _) }
  L_POpenParenthesis { PT _ (T_POpenParenthesis _) }
  L_PCloseParenthesis { PT _ (T_PCloseParenthesis _) }
  L_POpenBracket { PT _ (T_POpenBracket _) }
  L_PCloseBracket { PT _ (T_PCloseBracket _) }
  L_PSemicolon { PT _ (T_PSemicolon _) }
  L_PColon { PT _ (T_PColon _) }
  L_PPoint { PT _ (T_PPoint _) }
  L_PIf { PT _ (T_PIf _) }
  L_PThen { PT _ (T_PThen _) }
  L_PElse { PT _ (T_PElse _) }
  L_Pdo { PT _ (T_Pdo _) }
  L_PWhile { PT _ (T_PWhile _) }
  L_PIntType { PT _ (T_PIntType _) }
  L_PRealType { PT _ (T_PRealType _) }
  L_PCharType { PT _ (T_PCharType _) }
  L_PBoolType { PT _ (T_PBoolType _) }
  L_PStringType { PT _ (T_PStringType _) }
  L_PBreak { PT _ (T_PBreak _) }
  L_PContinue { PT _ (T_PContinue _) }
  L_PAssignmEq { PT _ (T_PAssignmEq _) }
  L_PRef { PT _ (T_PRef _) }
  L_PVar { PT _ (T_PVar _) }
  L_PProc { PT _ (T_PProc _) }
  L_PReturn { PT _ (T_PReturn _) }
  L_PTrue { PT _ (T_PTrue _) }
  L_PFalse { PT _ (T_PFalse _) }
  L_PEpow { PT _ (T_PEpow _) }
  L_PElthen { PT _ (T_PElthen _) }
  L_PEgrthen { PT _ (T_PEgrthen _) }
  L_PEplus { PT _ (T_PEplus _) }
  L_PEminus { PT _ (T_PEminus _) }
  L_PEtimes { PT _ (T_PEtimes _) }
  L_PEdiv { PT _ (T_PEdiv _) }
  L_PEmod { PT _ (T_PEmod _) }
  L_PDef { PT _ (T_PDef _) }
  L_PNeg { PT _ (T_PNeg _) }
  L_PElor { PT _ (T_PElor _) }
  L_PEland { PT _ (T_PEland _) }
  L_PEeq { PT _ (T_PEeq _) }
  L_PEneq { PT _ (T_PEneq _) }
  L_PEle { PT _ (T_PEle _) }
  L_PEge { PT _ (T_PEge _) }
  L_PAssignmPlus { PT _ (T_PAssignmPlus _) }
  L_PIdent { PT _ (T_PIdent _) }
  L_PString { PT _ (T_PString _) }
  L_PChar { PT _ (T_PChar _) }
  L_PDouble { PT _ (T_PDouble _) }
  L_PInteger { PT _ (T_PInteger _) }

%%

POpenGraph :: { POpenGraph}
POpenGraph  : L_POpenGraph { POpenGraph (mkPosToken $1)}

PCloseGraph :: { PCloseGraph}
PCloseGraph  : L_PCloseGraph { PCloseGraph (mkPosToken $1)}

POpenParenthesis :: { POpenParenthesis}
POpenParenthesis  : L_POpenParenthesis { POpenParenthesis (mkPosToken $1)}

PCloseParenthesis :: { PCloseParenthesis}
PCloseParenthesis  : L_PCloseParenthesis { PCloseParenthesis (mkPosToken $1)}

POpenBracket :: { POpenBracket}
POpenBracket  : L_POpenBracket { POpenBracket (mkPosToken $1)}

PCloseBracket :: { PCloseBracket}
PCloseBracket  : L_PCloseBracket { PCloseBracket (mkPosToken $1)}

PSemicolon :: { PSemicolon}
PSemicolon  : L_PSemicolon { PSemicolon (mkPosToken $1)}

PColon :: { PColon}
PColon  : L_PColon { PColon (mkPosToken $1)}

PPoint :: { PPoint}
PPoint  : L_PPoint { PPoint (mkPosToken $1)}

PIf :: { PIf}
PIf  : L_PIf { PIf (mkPosToken $1)}

PThen :: { PThen}
PThen  : L_PThen { PThen (mkPosToken $1)}

PElse :: { PElse}
PElse  : L_PElse { PElse (mkPosToken $1)}

Pdo :: { Pdo}
Pdo  : L_Pdo { Pdo (mkPosToken $1)}

PWhile :: { PWhile}
PWhile  : L_PWhile { PWhile (mkPosToken $1)}

PIntType :: { PIntType}
PIntType  : L_PIntType { PIntType (mkPosToken $1)}

PRealType :: { PRealType}
PRealType  : L_PRealType { PRealType (mkPosToken $1)}

PCharType :: { PCharType}
PCharType  : L_PCharType { PCharType (mkPosToken $1)}

PBoolType :: { PBoolType}
PBoolType  : L_PBoolType { PBoolType (mkPosToken $1)}

PStringType :: { PStringType}
PStringType  : L_PStringType { PStringType (mkPosToken $1)}

PBreak :: { PBreak}
PBreak  : L_PBreak { PBreak (mkPosToken $1)}

PContinue :: { PContinue}
PContinue  : L_PContinue { PContinue (mkPosToken $1)}

PAssignmEq :: { PAssignmEq}
PAssignmEq  : L_PAssignmEq { PAssignmEq (mkPosToken $1)}

PRef :: { PRef}
PRef  : L_PRef { PRef (mkPosToken $1)}

PVar :: { PVar}
PVar  : L_PVar { PVar (mkPosToken $1)}

PProc :: { PProc}
PProc  : L_PProc { PProc (mkPosToken $1)}

PReturn :: { PReturn}
PReturn  : L_PReturn { PReturn (mkPosToken $1)}

PTrue :: { PTrue}
PTrue  : L_PTrue { PTrue (mkPosToken $1)}

PFalse :: { PFalse}
PFalse  : L_PFalse { PFalse (mkPosToken $1)}

PEpow :: { PEpow}
PEpow  : L_PEpow { PEpow (mkPosToken $1)}

PElthen :: { PElthen}
PElthen  : L_PElthen { PElthen (mkPosToken $1)}

PEgrthen :: { PEgrthen}
PEgrthen  : L_PEgrthen { PEgrthen (mkPosToken $1)}

PEplus :: { PEplus}
PEplus  : L_PEplus { PEplus (mkPosToken $1)}

PEminus :: { PEminus}
PEminus  : L_PEminus { PEminus (mkPosToken $1)}

PEtimes :: { PEtimes}
PEtimes  : L_PEtimes { PEtimes (mkPosToken $1)}

PEdiv :: { PEdiv}
PEdiv  : L_PEdiv { PEdiv (mkPosToken $1)}

PEmod :: { PEmod}
PEmod  : L_PEmod { PEmod (mkPosToken $1)}

PDef :: { PDef}
PDef  : L_PDef { PDef (mkPosToken $1)}

PNeg :: { PNeg}
PNeg  : L_PNeg { PNeg (mkPosToken $1)}

PElor :: { PElor}
PElor  : L_PElor { PElor (mkPosToken $1)}

PEland :: { PEland}
PEland  : L_PEland { PEland (mkPosToken $1)}

PEeq :: { PEeq}
PEeq  : L_PEeq { PEeq (mkPosToken $1)}

PEneq :: { PEneq}
PEneq  : L_PEneq { PEneq (mkPosToken $1)}

PEle :: { PEle}
PEle  : L_PEle { PEle (mkPosToken $1)}

PEge :: { PEge}
PEge  : L_PEge { PEge (mkPosToken $1)}

PAssignmPlus :: { PAssignmPlus}
PAssignmPlus  : L_PAssignmPlus { PAssignmPlus (mkPosToken $1)}

PIdent :: { PIdent}
PIdent  : L_PIdent { PIdent (mkPosToken $1)}

PString :: { PString}
PString  : L_PString { PString (mkPosToken $1)}

PChar :: { PChar}
PChar  : L_PChar { PChar (mkPosToken $1)}

PDouble :: { PDouble}
PDouble  : L_PDouble { PDouble (mkPosToken $1)}

PInteger :: { PInteger}
PInteger  : L_PInteger { PInteger (mkPosToken $1)}

Program :: { Program }
Program : Module { AbsChapel.Progr $1 }
Module :: { Module }
Module : ListExt { AbsChapel.Mod (reverse $1) }
ListExt :: { [Ext] }
ListExt : {- empty -} { [] } | ListExt Ext { flip (:) $1 $2 }
Ext :: { Ext }
Ext : Declaration { AbsChapel.ExtDecl $1 }
    | Function { AbsChapel.ExtFun $1 }
Declaration :: { Declaration }
Declaration : DecMode ListDeclList PSemicolon { AbsChapel.Decl $1 $2 $3 }
ListDeclList :: { [DeclList] }
ListDeclList : DeclList { (:[]) $1 }
             | DeclList ',' ListDeclList { (:) $1 $3 }
DeclList :: { DeclList }
DeclList : ListPIdent PColon TypeSpec { AbsChapel.NoAssgmArrayDec $1 $2 $3 }
         | ListPIdent PColon TypeSpec AssgnmOp ExprDecl { AbsChapel.AssgmTypeDec $1 $2 $3 $4 $5 }
         | ListPIdent AssgnmOp ExprDecl { AbsChapel.AssgmDec $1 $2 $3 }
TypeSpec :: { TypeSpec }
TypeSpec : Type { AbsChapel.TypeSpecNorm $1 }
         | ArDecl Type { AbsChapel.TypeSpecAr $1 $2 }
ExprDecl :: { ExprDecl }
ExprDecl : ArInit { AbsChapel.ExprDecArray $1 }
         | Exp { AbsChapel.ExprDec $1 }
ArInit :: { ArInit }
ArInit : POpenBracket ListExprDecl PCloseBracket { AbsChapel.ArrayInit $1 $2 $3 }
ListExprDecl :: { [ExprDecl] }
ListExprDecl : ExprDecl { (:[]) $1 }
             | ExprDecl ',' ListExprDecl { (:) $1 $3 }
ArDecl :: { ArDecl }
ArDecl : POpenBracket ListArDim PCloseBracket { AbsChapel.ArrayDeclIndex $1 $2 $3 }
ArDim :: { ArDim }
ArDim : ArBound PPoint PPoint ArBound { AbsChapel.ArrayDimSingle $1 $2 $3 $4 }
      | ArBound { AbsChapel.ArrayDimBound $1 }
ListArDim :: { [ArDim] }
ListArDim : ArDim { (:[]) $1 } | ArDim ',' ListArDim { (:) $1 $3 }
ArBound :: { ArBound }
ArBound : PIdent { AbsChapel.ArrayBoundIdent $1 }
        | Constant { AbsChapel.ArratBoundConst $1 }
ListPIdent :: { [PIdent] }
ListPIdent : PIdent { (:[]) $1 }
           | PIdent ',' ListPIdent { (:) $1 $3 }
DecMode :: { DecMode }
DecMode : PVar { AbsChapel.PVarMode $1 }
Function :: { Function }
Function : PProc Signature Body { AbsChapel.FunDec $1 $2 $3 }
Signature :: { Signature }
Signature : PIdent FunctionParams { AbsChapel.SignNoRet $1 $2 }
          | PIdent FunctionParams PColon TypeSpec { AbsChapel.SignWRet $1 $2 $3 $4 }
FunctionParams :: { FunctionParams }
FunctionParams : POpenParenthesis ListParam PCloseParenthesis { AbsChapel.FunParams $1 $2 $3 }
ListParam :: { [Param] }
ListParam : {- empty -} { [] }
          | Param { (:[]) $1 }
          | Param ',' ListParam { (:) $1 $3 }
Param :: { Param }
Param : ListPIdent PColon TypeSpec { AbsChapel.ParNoMode $1 $2 $3 }
      | Mode ListPIdent PColon TypeSpec { AbsChapel.ParWMode $1 $2 $3 $4 }
ListPassedParam :: { [PassedParam] }
ListPassedParam : {- empty -} { [] }
                | PassedParam { (:[]) $1 }
                | PassedParam ',' ListPassedParam { (:) $1 $3 }
PassedParam :: { PassedParam }
PassedParam : Exp { AbsChapel.PassedPar $1 }
            | Mode Exp { AbsChapel.PassedParWMode $1 $2 }
Body :: { Body }
Body : POpenGraph ListBodyStatement PCloseGraph { AbsChapel.BodyBlock $1 (reverse $2) $3 }
ListBodyStatement :: { [BodyStatement] }
ListBodyStatement : {- empty -} { [] }
                  | ListBodyStatement BodyStatement { flip (:) $1 $2 }
BodyStatement :: { BodyStatement }
BodyStatement : Statement { AbsChapel.Stm $1 }
              | Function PSemicolon { AbsChapel.Fun $1 $2 }
              | Declaration { AbsChapel.DeclStm $1 }
              | Body { AbsChapel.Block $1 }
Statement :: { Statement }
Statement : Pdo PWhile Body Guard { AbsChapel.DoWhile $1 $2 $3 $4 }
          | PWhile Guard Body { AbsChapel.While $1 $2 $3 }
          | PIf Guard PThen Body { AbsChapel.If $1 $2 $3 $4 }
          | PIf Guard PThen Body PElse Body { AbsChapel.IfElse $1 $2 $3 $4 $5 $6 }
          | PReturn Exp PSemicolon { AbsChapel.RetVal $1 $2 $3 }
          | PReturn PSemicolon { AbsChapel.RetVoid $1 $2 }
          | PContinue PSemicolon { AbsChapel.Continue $1 $2 }
          | PBreak PSemicolon { AbsChapel.Break $1 $2 }
          | Exp PSemicolon { AbsChapel.StExp $1 $2 }
Guard :: { Guard }
Guard : POpenParenthesis Exp PCloseParenthesis { AbsChapel.SGuard $1 $2 $3 }
Type :: { Type }
Type : PIntType { AbsChapel.Tint $1 }
     | PRealType { AbsChapel.Treal $1 }
     | PCharType { AbsChapel.Tchar $1 }
     | PStringType { AbsChapel.Tstring $1 }
     | PBoolType { AbsChapel.Tbool $1 }
     | PEtimes Type { AbsChapel.TPointer $1 $2 }
AssgnmOp :: { AssgnmOp }
AssgnmOp : PAssignmEq { AbsChapel.AssgnEq $1 }
         | PAssignmPlus { AbsChapel.AssgnPlEq $1 }
Mode :: { Mode }
Mode : PRef { AbsChapel.RefMode $1 }
Exp :: { Exp }
Exp : Exp AssgnmOp Exp1 { AbsChapel.EAss $1 $2 $3 } | Exp1 { $1 }
Exp1 :: { Exp }
Exp1 : Exp1 PElor Exp2 { AbsChapel.Elor $1 $2 $3 } | Exp2 { $1 }
Exp2 :: { Exp }
Exp2 : Exp2 PEland Exp3 { AbsChapel.Eland $1 $2 $3 } | Exp3 { $1 }
Exp3 :: { Exp }
Exp3 : Exp3 PEeq Exp4 { AbsChapel.Eeq $1 $2 $3 }
     | Exp3 PEneq Exp4 { AbsChapel.Eneq $1 $2 $3 }
     | Exp4 { $1 }
Exp4 :: { Exp }
Exp4 : Exp4 PElthen Exp5 { AbsChapel.Elthen $1 $2 $3 }
     | Exp4 PEgrthen Exp5 { AbsChapel.Egrthen $1 $2 $3 }
     | Exp4 PEle Exp5 { AbsChapel.Ele $1 $2 $3 }
     | Exp4 PEge Exp5 { AbsChapel.Ege $1 $2 $3 }
     | Exp5 { $1 }
Exp5 :: { Exp }
Exp5 : Exp5 PEplus Exp6 { AbsChapel.Eplus $1 $2 $3 }
     | Exp5 PEminus Exp6 { AbsChapel.Eminus $1 $2 $3 }
     | Exp6 { $1 }
Exp6 :: { Exp }
Exp6 : Exp6 PEtimes Exp7 { AbsChapel.Etimes $1 $2 $3 }
     | Exp6 PEdiv Exp7 { AbsChapel.Ediv $1 $2 $3 }
     | Exp6 PEmod Exp7 { AbsChapel.Emod $1 $2 $3 }
     | Exp7 { $1 }
Exp7 :: { Exp }
Exp7 : UnaryOperator Exp7 { AbsChapel.Epreop $1 $2 } | Exp8 { $1 }
Exp8 :: { Exp }
Exp8 : Exp8 PEpow Exp9 { AbsChapel.Epow $1 $2 $3 }
     | Exp9 ArInit { AbsChapel.Earray $1 $2 }
     | Exp9 { $1 }
Exp9 :: { Exp }
Exp9 : POpenParenthesis Exp PCloseParenthesis { AbsChapel.InnerExp $1 $2 $3 }
     | PIdent POpenParenthesis ListPassedParam PCloseParenthesis { AbsChapel.EFun $1 $2 $3 $4 }
     | PIdent { AbsChapel.Evar $1 }
     | Constant { AbsChapel.Econst $1 }
UnaryOperator :: { UnaryOperator }
UnaryOperator : PNeg { AbsChapel.Negation $1 }
              | PEminus { AbsChapel.MinusUnary $1 }
              | PDef { AbsChapel.Address $1 }
              | PEtimes { AbsChapel.Indirection $1 }
Constant :: { Constant }
Constant : PString { AbsChapel.Estring $1 }
         | PDouble { AbsChapel.Efloat $1 }
         | PChar { AbsChapel.Echar $1 }
         | PInteger { AbsChapel.Eint $1 }
         | PTrue { AbsChapel.ETrue $1 }
         | PFalse { AbsChapel.EFalse $1 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
}

