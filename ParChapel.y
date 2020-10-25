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
  L_PInt { PT _ (T_PInt _) }
  L_PReal { PT _ (T_PReal _) }
  L_PAssignmEq { PT _ (T_PAssignmEq _) }
  L_PAssignmPlus { PT _ (T_PAssignmPlus _) }
  L_PRef { PT _ (T_PRef _) }
  L_PVar { PT _ (T_PVar _) }
  L_PConst { PT _ (T_PConst _) }
  L_PProc { PT _ (T_PProc _) }
  L_PReturn { PT _ (T_PReturn _) }
  L_PElthen { PT _ (T_PElthen _) }
  L_PEgrthen { PT _ (T_PEgrthen _) }
  L_PEplus { PT _ (T_PEplus _) }
  L_PEminus { PT _ (T_PEminus _) }
  L_PEtimes { PT _ (T_PEtimes _) }
  L_PEdiv { PT _ (T_PEdiv _) }
  L_PEmod { PT _ (T_PEmod _) }
  L_PDef { PT _ (T_PDef _) }
  L_PElor { PT _ (T_PElor _) }
  L_PEland { PT _ (T_PEland _) }
  L_PEeq { PT _ (T_PEeq _) }
  L_PEneq { PT _ (T_PEneq _) }
  L_PEle { PT _ (T_PEle _) }
  L_PEge { PT _ (T_PEge _) }
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

PInt :: { PInt}
PInt  : L_PInt { PInt (mkPosToken $1)}

PReal :: { PReal}
PReal  : L_PReal { PReal (mkPosToken $1)}

PAssignmEq :: { PAssignmEq}
PAssignmEq  : L_PAssignmEq { PAssignmEq (mkPosToken $1)}

PAssignmPlus :: { PAssignmPlus}
PAssignmPlus  : L_PAssignmPlus { PAssignmPlus (mkPosToken $1)}

PRef :: { PRef}
PRef  : L_PRef { PRef (mkPosToken $1)}

PVar :: { PVar}
PVar  : L_PVar { PVar (mkPosToken $1)}

PConst :: { PConst}
PConst  : L_PConst { PConst (mkPosToken $1)}

PProc :: { PProc}
PProc  : L_PProc { PProc (mkPosToken $1)}

PReturn :: { PReturn}
PReturn  : L_PReturn { PReturn (mkPosToken $1)}

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
DeclList : ListPIdent PColon Type { AbsChapel.NoAssgmDec $1 $2 $3 }
         | ListPIdent PColon ArDecl { AbsChapel.NoAssgmArrayFixDec $1 $2 $3 }
         | ListPIdent PColon ArDecl Type { AbsChapel.NoAssgmArrayDec $1 $2 $3 $4 }
         | ListPIdent PColon Type PAssignmEq ExprDecl { AbsChapel.AssgmTypeDec $1 $2 $3 $4 $5 }
         | ListPIdent PColon ArDecl Type PAssignmEq ExprDecl { AbsChapel.AssgmArrayTypeDec $1 $2 $3 $4 $5 $6 }
         | ListPIdent PColon ArDecl PAssignmEq ExprDecl { AbsChapel.AssgmArrayDec $1 $2 $3 $4 $5 }
         | ListPIdent PAssignmEq ExprDecl { AbsChapel.AssgmDec $1 $2 $3 }
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
        | PConst { AbsChapel.PConstMode $1 }
Function :: { Function }
Function : PProc Signature Body { AbsChapel.FunDec $1 $2 $3 }
Signature :: { Signature }
Signature : PIdent FunctionParams { AbsChapel.SignNoRet $1 $2 }
          | PIdent FunctionParams PColon Type { AbsChapel.SignWRet $1 $2 $3 $4 }
FunctionParams :: { FunctionParams }
FunctionParams : POpenParenthesis ListParam PCloseParenthesis { AbsChapel.FunParams $1 $2 $3 }
ListParam :: { [Param] }
ListParam : {- empty -} { [] }
          | Param { (:[]) $1 }
          | Param ',' ListParam { (:) $1 $3 }
Param :: { Param }
Param : ListPIdent PColon Type { AbsChapel.ParNoMode $1 $2 $3 }
      | Mode ListPIdent PColon Type { AbsChapel.ParWMode $1 $2 $3 $4 }
ListPassedParam :: { [PassedParam] }
ListPassedParam : {- empty -} { [] }
                | PassedParam { (:[]) $1 }
                | PassedParam ',' ListPassedParam { (:) $1 $3 }
PassedParam :: { PassedParam }
PassedParam : Exp { AbsChapel.PassedPar $1 }
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
          | Exp PSemicolon { AbsChapel.StExp $1 $2 }
Guard :: { Guard }
Guard : POpenParenthesis Exp PCloseParenthesis { AbsChapel.SGuard $1 $2 $3 }
Type :: { Type }
Type : PInt { AbsChapel.Tint $1 } | PReal { AbsChapel.Treal $1 }
AssgnmOp :: { AssgnmOp }
AssgnmOp : PAssignmEq { AbsChapel.AssgnEq $1 }
         | PAssignmPlus { AbsChapel.AssgnPlEq $1 }
Mode :: { Mode }
Mode : PRef { AbsChapel.RefMode $1 }
Exp :: { Exp }
Exp : Exp AssgnmOp Exp4 { AbsChapel.EAss $1 $2 $3 } | Exp4 { $1 }
Exp4 :: { Exp }
Exp4 : Exp4 PElor Exp5 { AbsChapel.Elor $1 $2 $3 } | Exp5 { $1 }
Exp5 :: { Exp }
Exp5 : Exp5 PEland Exp8 { AbsChapel.Eland $1 $2 $3 } | Exp6 { $1 }
Exp8 :: { Exp }
Exp8 : Exp8 PDef Exp9 { AbsChapel.Ebitand $1 $2 $3 } | Exp9 { $1 }
Exp9 :: { Exp }
Exp9 : Exp9 PEeq Exp10 { AbsChapel.Eeq $1 $2 $3 }
     | Exp9 PEneq Exp10 { AbsChapel.Eneq $1 $2 $3 }
     | Exp10 { $1 }
Exp10 :: { Exp }
Exp10 : Exp10 PElthen Exp11 { AbsChapel.Elthen $1 $2 $3 }
      | Exp10 PEgrthen Exp11 { AbsChapel.Egrthen $1 $2 $3 }
      | Exp10 PEle Exp11 { AbsChapel.Ele $1 $2 $3 }
      | Exp10 PEge Exp12 { AbsChapel.Ege $1 $2 $3 }
      | Exp11 { $1 }
Exp12 :: { Exp }
Exp12 : Exp12 PEplus Exp13 { AbsChapel.Eplus $1 $2 $3 }
      | Exp12 PEminus Exp13 { AbsChapel.Eminus $1 $2 $3 }
      | Exp13 { $1 }
Exp13 :: { Exp }
Exp13 : Exp13 PEtimes Exp14 { AbsChapel.Etimes $1 $2 $3 }
      | Exp13 PEdiv Exp14 { AbsChapel.Ediv $1 $2 $3 }
      | Exp13 PEmod Exp14 { AbsChapel.Emod $1 $2 $3 }
      | Exp14 { $1 }
Exp14 :: { Exp }
Exp14 : UnaryOperator Exp14 { AbsChapel.Epreop $1 $2 }
      | Exp15 { $1 }
Exp15 :: { Exp }
Exp15 : Exp15 ArInit { AbsChapel.Earray $1 $2 } | Exp16 { $1 }
Exp6 :: { Exp }
Exp6 : Exp7 { $1 }
Exp7 :: { Exp }
Exp7 : Exp8 { $1 }
Exp11 :: { Exp }
Exp11 : Exp12 { $1 }
Exp16 :: { Exp }
Exp16 : POpenParenthesis Exp PCloseParenthesis { AbsChapel.InnerExp $1 $2 $3 }
      | PIdent POpenParenthesis ListPassedParam PCloseParenthesis { AbsChapel.EFun $1 $2 $3 $4 }
      | PIdent { AbsChapel.Evar $1 }
      | Constant { AbsChapel.Econst $1 }
      | PString { AbsChapel.Estring $1 }
UnaryOperator :: { UnaryOperator }
UnaryOperator : PDef { AbsChapel.Address $1 }
              | PEtimes { AbsChapel.Indirection $1 }
Constant :: { Constant }
Constant : PDouble { AbsChapel.Efloat $1 }
         | PChar { AbsChapel.Echar $1 }
         | PInteger { AbsChapel.Eint $1 }
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

