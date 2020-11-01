{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintChapel.
--   Generated by the BNF converter.

module PrintChapel where

import qualified AbsChapel
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsChapel.POpenGraph where
  prt _ (AbsChapel.POpenGraph (_,i)) = doc (showString i)

instance Print AbsChapel.PCloseGraph where
  prt _ (AbsChapel.PCloseGraph (_,i)) = doc (showString i)

instance Print AbsChapel.POpenParenthesis where
  prt _ (AbsChapel.POpenParenthesis (_,i)) = doc (showString i)

instance Print AbsChapel.PCloseParenthesis where
  prt _ (AbsChapel.PCloseParenthesis (_,i)) = doc (showString i)

instance Print AbsChapel.POpenBracket where
  prt _ (AbsChapel.POpenBracket (_,i)) = doc (showString i)

instance Print AbsChapel.PCloseBracket where
  prt _ (AbsChapel.PCloseBracket (_,i)) = doc (showString i)

instance Print AbsChapel.PSemicolon where
  prt _ (AbsChapel.PSemicolon (_,i)) = doc (showString i)

instance Print AbsChapel.PColon where
  prt _ (AbsChapel.PColon (_,i)) = doc (showString i)

instance Print AbsChapel.PPoint where
  prt _ (AbsChapel.PPoint (_,i)) = doc (showString i)

instance Print AbsChapel.PIf where
  prt _ (AbsChapel.PIf (_,i)) = doc (showString i)

instance Print AbsChapel.PThen where
  prt _ (AbsChapel.PThen (_,i)) = doc (showString i)

instance Print AbsChapel.PElse where
  prt _ (AbsChapel.PElse (_,i)) = doc (showString i)

instance Print AbsChapel.Pdo where
  prt _ (AbsChapel.Pdo (_,i)) = doc (showString i)

instance Print AbsChapel.PWhile where
  prt _ (AbsChapel.PWhile (_,i)) = doc (showString i)

instance Print AbsChapel.PIntType where
  prt _ (AbsChapel.PIntType (_,i)) = doc (showString i)

instance Print AbsChapel.PRealType where
  prt _ (AbsChapel.PRealType (_,i)) = doc (showString i)

instance Print AbsChapel.PCharType where
  prt _ (AbsChapel.PCharType (_,i)) = doc (showString i)

instance Print AbsChapel.PBoolType where
  prt _ (AbsChapel.PBoolType (_,i)) = doc (showString i)

instance Print AbsChapel.PStringType where
  prt _ (AbsChapel.PStringType (_,i)) = doc (showString i)

instance Print AbsChapel.PBreak where
  prt _ (AbsChapel.PBreak (_,i)) = doc (showString i)

instance Print AbsChapel.PContinue where
  prt _ (AbsChapel.PContinue (_,i)) = doc (showString i)

instance Print AbsChapel.PAssignmEq where
  prt _ (AbsChapel.PAssignmEq (_,i)) = doc (showString i)

instance Print AbsChapel.PRef where
  prt _ (AbsChapel.PRef (_,i)) = doc (showString i)

instance Print AbsChapel.PVar where
  prt _ (AbsChapel.PVar (_,i)) = doc (showString i)

instance Print AbsChapel.PConst where
  prt _ (AbsChapel.PConst (_,i)) = doc (showString i)

instance Print AbsChapel.PProc where
  prt _ (AbsChapel.PProc (_,i)) = doc (showString i)

instance Print AbsChapel.PReturn where
  prt _ (AbsChapel.PReturn (_,i)) = doc (showString i)

instance Print AbsChapel.PTrue where
  prt _ (AbsChapel.PTrue (_,i)) = doc (showString i)

instance Print AbsChapel.PFalse where
  prt _ (AbsChapel.PFalse (_,i)) = doc (showString i)

instance Print AbsChapel.PElthen where
  prt _ (AbsChapel.PElthen (_,i)) = doc (showString i)

instance Print AbsChapel.PEgrthen where
  prt _ (AbsChapel.PEgrthen (_,i)) = doc (showString i)

instance Print AbsChapel.PEplus where
  prt _ (AbsChapel.PEplus (_,i)) = doc (showString i)

instance Print AbsChapel.PEminus where
  prt _ (AbsChapel.PEminus (_,i)) = doc (showString i)

instance Print AbsChapel.PEtimes where
  prt _ (AbsChapel.PEtimes (_,i)) = doc (showString i)

instance Print AbsChapel.PEdiv where
  prt _ (AbsChapel.PEdiv (_,i)) = doc (showString i)

instance Print AbsChapel.PEmod where
  prt _ (AbsChapel.PEmod (_,i)) = doc (showString i)

instance Print AbsChapel.PDef where
  prt _ (AbsChapel.PDef (_,i)) = doc (showString i)

instance Print AbsChapel.PElor where
  prt _ (AbsChapel.PElor (_,i)) = doc (showString i)

instance Print AbsChapel.PEland where
  prt _ (AbsChapel.PEland (_,i)) = doc (showString i)

instance Print AbsChapel.PEeq where
  prt _ (AbsChapel.PEeq (_,i)) = doc (showString i)

instance Print AbsChapel.PEneq where
  prt _ (AbsChapel.PEneq (_,i)) = doc (showString i)

instance Print AbsChapel.PEle where
  prt _ (AbsChapel.PEle (_,i)) = doc (showString i)

instance Print AbsChapel.PEge where
  prt _ (AbsChapel.PEge (_,i)) = doc (showString i)

instance Print AbsChapel.PAssignmPlus where
  prt _ (AbsChapel.PAssignmPlus (_,i)) = doc (showString i)

instance Print AbsChapel.PIdent where
  prt _ (AbsChapel.PIdent (_,i)) = doc (showString i)
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsChapel.PString where
  prt _ (AbsChapel.PString (_,i)) = doc (showString i)

instance Print AbsChapel.PChar where
  prt _ (AbsChapel.PChar (_,i)) = doc (showString i)

instance Print AbsChapel.PDouble where
  prt _ (AbsChapel.PDouble (_,i)) = doc (showString i)

instance Print AbsChapel.PInteger where
  prt _ (AbsChapel.PInteger (_,i)) = doc (showString i)

instance Print AbsChapel.Program where
  prt i e = case e of
    AbsChapel.Progr module_ -> prPrec i 0 (concatD [prt 0 module_])

instance Print AbsChapel.Module where
  prt i e = case e of
    AbsChapel.Mod exts -> prPrec i 0 (concatD [prt 0 exts])

instance Print [AbsChapel.Ext] where
  prt = prtList

instance Print AbsChapel.Ext where
  prt i e = case e of
    AbsChapel.ExtDecl declaration -> prPrec i 0 (concatD [prt 0 declaration])
    AbsChapel.ExtFun function -> prPrec i 0 (concatD [prt 0 function])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsChapel.Declaration where
  prt i e = case e of
    AbsChapel.Decl decmode decllists psemicolon -> prPrec i 0 (concatD [prt 0 decmode, prt 0 decllists, prt 0 psemicolon])

instance Print [AbsChapel.DeclList] where
  prt = prtList

instance Print AbsChapel.DeclList where
  prt i e = case e of
    AbsChapel.NoAssgmDec pidents pcolon type_ -> prPrec i 0 (concatD [prt 0 pidents, prt 0 pcolon, prt 0 type_])
    AbsChapel.NoAssgmArrayFixDec pidents pcolon ardecl -> prPrec i 0 (concatD [prt 0 pidents, prt 0 pcolon, prt 0 ardecl])
    AbsChapel.NoAssgmArrayDec pidents pcolon ardecl type_ -> prPrec i 0 (concatD [prt 0 pidents, prt 0 pcolon, prt 0 ardecl, prt 0 type_])
    AbsChapel.AssgmTypeDec pidents pcolon type_ assgnmop exprdecl -> prPrec i 0 (concatD [prt 0 pidents, prt 0 pcolon, prt 0 type_, prt 0 assgnmop, prt 0 exprdecl])
    AbsChapel.AssgmArrayTypeDec pidents pcolon ardecl type_ assgnmop exprdecl -> prPrec i 0 (concatD [prt 0 pidents, prt 0 pcolon, prt 0 ardecl, prt 0 type_, prt 0 assgnmop, prt 0 exprdecl])
    AbsChapel.AssgmArrayDec pidents pcolon ardecl assgnmop exprdecl -> prPrec i 0 (concatD [prt 0 pidents, prt 0 pcolon, prt 0 ardecl, prt 0 assgnmop, prt 0 exprdecl])
    AbsChapel.AssgmDec pidents assgnmop exprdecl -> prPrec i 0 (concatD [prt 0 pidents, prt 0 assgnmop, prt 0 exprdecl])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsChapel.ExprDecl where
  prt i e = case e of
    AbsChapel.ExprDecArray arinit -> prPrec i 0 (concatD [prt 0 arinit])
    AbsChapel.ExprDec exp -> prPrec i 0 (concatD [prt 0 exp])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsChapel.ArInit where
  prt i e = case e of
    AbsChapel.ArrayInit popenbracket exprdecls pclosebracket -> prPrec i 0 (concatD [prt 0 popenbracket, prt 0 exprdecls, prt 0 pclosebracket])

instance Print [AbsChapel.ExprDecl] where
  prt = prtList

instance Print AbsChapel.ArDecl where
  prt i e = case e of
    AbsChapel.ArrayDeclIndex popenbracket ardims pclosebracket -> prPrec i 0 (concatD [prt 0 popenbracket, prt 0 ardims, prt 0 pclosebracket])

instance Print AbsChapel.ArDim where
  prt i e = case e of
    AbsChapel.ArrayDimSingle arbound1 ppoint1 ppoint2 arbound2 -> prPrec i 0 (concatD [prt 0 arbound1, prt 0 ppoint1, prt 0 ppoint2, prt 0 arbound2])
    AbsChapel.ArrayDimBound arbound -> prPrec i 0 (concatD [prt 0 arbound])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsChapel.ArDim] where
  prt = prtList

instance Print AbsChapel.ArBound where
  prt i e = case e of
    AbsChapel.ArrayBoundIdent pident -> prPrec i 0 (concatD [prt 0 pident])
    AbsChapel.ArratBoundConst constant -> prPrec i 0 (concatD [prt 0 constant])

instance Print [AbsChapel.PIdent] where
  prt = prtList

instance Print AbsChapel.DecMode where
  prt i e = case e of
    AbsChapel.PVarMode pvar -> prPrec i 0 (concatD [prt 0 pvar])
    AbsChapel.PConstMode pconst -> prPrec i 0 (concatD [prt 0 pconst])

instance Print AbsChapel.Function where
  prt i e = case e of
    AbsChapel.FunDec pproc signature body -> prPrec i 0 (concatD [prt 0 pproc, prt 0 signature, prt 0 body])

instance Print AbsChapel.Signature where
  prt i e = case e of
    AbsChapel.SignNoRet pident functionparams -> prPrec i 0 (concatD [prt 0 pident, prt 0 functionparams])
    AbsChapel.SignWRet pident functionparams pcolon type_ -> prPrec i 0 (concatD [prt 0 pident, prt 0 functionparams, prt 0 pcolon, prt 0 type_])

instance Print AbsChapel.FunctionParams where
  prt i e = case e of
    AbsChapel.FunParams popenparenthesis params pcloseparenthesis -> prPrec i 0 (concatD [prt 0 popenparenthesis, prt 0 params, prt 0 pcloseparenthesis])

instance Print [AbsChapel.Param] where
  prt = prtList

instance Print AbsChapel.Param where
  prt i e = case e of
    AbsChapel.ParNoMode pidents pcolon type_ -> prPrec i 0 (concatD [prt 0 pidents, prt 0 pcolon, prt 0 type_])
    AbsChapel.ParWMode mode pidents pcolon type_ -> prPrec i 0 (concatD [prt 0 mode, prt 0 pidents, prt 0 pcolon, prt 0 type_])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsChapel.PassedParam] where
  prt = prtList

instance Print AbsChapel.PassedParam where
  prt i e = case e of
    AbsChapel.PassedPar exp -> prPrec i 0 (concatD [prt 0 exp])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsChapel.Body where
  prt i e = case e of
    AbsChapel.BodyBlock popengraph bodystatements pclosegraph -> prPrec i 0 (concatD [prt 0 popengraph, prt 0 bodystatements, prt 0 pclosegraph])

instance Print [AbsChapel.BodyStatement] where
  prt = prtList

instance Print AbsChapel.BodyStatement where
  prt i e = case e of
    AbsChapel.Stm statement -> prPrec i 0 (concatD [prt 0 statement])
    AbsChapel.Fun function psemicolon -> prPrec i 0 (concatD [prt 0 function, prt 0 psemicolon])
    AbsChapel.DeclStm declaration -> prPrec i 0 (concatD [prt 0 declaration])
    AbsChapel.Block body -> prPrec i 0 (concatD [prt 0 body])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsChapel.Statement where
  prt i e = case e of
    AbsChapel.DoWhile pdo pwhile body guard -> prPrec i 0 (concatD [prt 0 pdo, prt 0 pwhile, prt 0 body, prt 0 guard])
    AbsChapel.While pwhile guard body -> prPrec i 0 (concatD [prt 0 pwhile, prt 0 guard, prt 0 body])
    AbsChapel.If pif guard pthen body -> prPrec i 0 (concatD [prt 0 pif, prt 0 guard, prt 0 pthen, prt 0 body])
    AbsChapel.IfElse pif guard pthen body1 pelse body2 -> prPrec i 0 (concatD [prt 0 pif, prt 0 guard, prt 0 pthen, prt 0 body1, prt 0 pelse, prt 0 body2])
    AbsChapel.RetVal preturn exp psemicolon -> prPrec i 0 (concatD [prt 0 preturn, prt 0 exp, prt 0 psemicolon])
    AbsChapel.RetVoid preturn psemicolon -> prPrec i 0 (concatD [prt 0 preturn, prt 0 psemicolon])
    AbsChapel.Continue pcontinue psemicolon -> prPrec i 0 (concatD [prt 0 pcontinue, prt 0 psemicolon])
    AbsChapel.Break pbreak psemicolon -> prPrec i 0 (concatD [prt 0 pbreak, prt 0 psemicolon])
    AbsChapel.StExp exp psemicolon -> prPrec i 0 (concatD [prt 0 exp, prt 0 psemicolon])

instance Print AbsChapel.Guard where
  prt i e = case e of
    AbsChapel.SGuard popenparenthesis exp pcloseparenthesis -> prPrec i 0 (concatD [prt 0 popenparenthesis, prt 0 exp, prt 0 pcloseparenthesis])

instance Print AbsChapel.Type where
  prt i e = case e of
    AbsChapel.Tint pinttype -> prPrec i 0 (concatD [prt 0 pinttype])
    AbsChapel.Treal prealtype -> prPrec i 0 (concatD [prt 0 prealtype])
    AbsChapel.Tchar pchartype -> prPrec i 0 (concatD [prt 0 pchartype])
    AbsChapel.Tstring pstringtype -> prPrec i 0 (concatD [prt 0 pstringtype])
    AbsChapel.Tbool pbooltype -> prPrec i 0 (concatD [prt 0 pbooltype])
    AbsChapel.TPointer petimes type_ -> prPrec i 0 (concatD [prt 0 petimes, prt 0 type_])

instance Print AbsChapel.AssgnmOp where
  prt i e = case e of
    AbsChapel.AssgnEq passignmeq -> prPrec i 0 (concatD [prt 0 passignmeq])
    AbsChapel.AssgnPlEq passignmplus -> prPrec i 0 (concatD [prt 0 passignmplus])

instance Print AbsChapel.Mode where
  prt i e = case e of
    AbsChapel.RefMode pref -> prPrec i 0 (concatD [prt 0 pref])

instance Print AbsChapel.Exp where
  prt i e = case e of
    AbsChapel.EAss exp1 assgnmop exp2 -> prPrec i 0 (concatD [prt 0 exp1, prt 0 assgnmop, prt 4 exp2])
    AbsChapel.Elor exp1 pelor exp2 -> prPrec i 4 (concatD [prt 4 exp1, prt 0 pelor, prt 5 exp2])
    AbsChapel.Eland exp1 peland exp2 -> prPrec i 5 (concatD [prt 5 exp1, prt 0 peland, prt 8 exp2])
    AbsChapel.Ebitand exp1 pdef exp2 -> prPrec i 8 (concatD [prt 8 exp1, prt 0 pdef, prt 9 exp2])
    AbsChapel.Eeq exp1 peeq exp2 -> prPrec i 9 (concatD [prt 9 exp1, prt 0 peeq, prt 10 exp2])
    AbsChapel.Eneq exp1 peneq exp2 -> prPrec i 9 (concatD [prt 9 exp1, prt 0 peneq, prt 10 exp2])
    AbsChapel.Elthen exp1 pelthen exp2 -> prPrec i 10 (concatD [prt 10 exp1, prt 0 pelthen, prt 11 exp2])
    AbsChapel.Egrthen exp1 pegrthen exp2 -> prPrec i 10 (concatD [prt 10 exp1, prt 0 pegrthen, prt 11 exp2])
    AbsChapel.Ele exp1 pele exp2 -> prPrec i 10 (concatD [prt 10 exp1, prt 0 pele, prt 11 exp2])
    AbsChapel.Ege exp1 pege exp2 -> prPrec i 10 (concatD [prt 10 exp1, prt 0 pege, prt 12 exp2])
    AbsChapel.Eplus exp1 peplus exp2 -> prPrec i 12 (concatD [prt 12 exp1, prt 0 peplus, prt 13 exp2])
    AbsChapel.Eminus exp1 peminus exp2 -> prPrec i 12 (concatD [prt 12 exp1, prt 0 peminus, prt 13 exp2])
    AbsChapel.Etimes exp1 petimes exp2 -> prPrec i 13 (concatD [prt 13 exp1, prt 0 petimes, prt 14 exp2])
    AbsChapel.Ediv exp1 pediv exp2 -> prPrec i 13 (concatD [prt 13 exp1, prt 0 pediv, prt 14 exp2])
    AbsChapel.Emod exp1 pemod exp2 -> prPrec i 13 (concatD [prt 13 exp1, prt 0 pemod, prt 14 exp2])
    AbsChapel.Epreop unaryoperator exp -> prPrec i 14 (concatD [prt 0 unaryoperator, prt 14 exp])
    AbsChapel.Earray exp arinit -> prPrec i 15 (concatD [prt 15 exp, prt 0 arinit])
    AbsChapel.InnerExp popenparenthesis exp pcloseparenthesis -> prPrec i 16 (concatD [prt 0 popenparenthesis, prt 0 exp, prt 0 pcloseparenthesis])
    AbsChapel.EFun pident popenparenthesis passedparams pcloseparenthesis -> prPrec i 16 (concatD [prt 0 pident, prt 0 popenparenthesis, prt 0 passedparams, prt 0 pcloseparenthesis])
    AbsChapel.Evar pident -> prPrec i 16 (concatD [prt 0 pident])
    AbsChapel.Econst constant -> prPrec i 16 (concatD [prt 0 constant])

instance Print AbsChapel.UnaryOperator where
  prt i e = case e of
    AbsChapel.Address pdef -> prPrec i 0 (concatD [prt 0 pdef])
    AbsChapel.Indirection petimes -> prPrec i 0 (concatD [prt 0 petimes])

instance Print AbsChapel.Constant where
  prt i e = case e of
    AbsChapel.Estring pstring -> prPrec i 0 (concatD [prt 0 pstring])
    AbsChapel.Efloat pdouble -> prPrec i 0 (concatD [prt 0 pdouble])
    AbsChapel.Echar pchar -> prPrec i 0 (concatD [prt 0 pchar])
    AbsChapel.Eint pinteger -> prPrec i 0 (concatD [prt 0 pinteger])
    AbsChapel.ETrue ptrue -> prPrec i 0 (concatD [prt 0 ptrue])
    AbsChapel.EFalse pfalse -> prPrec i 0 (concatD [prt 0 pfalse])

