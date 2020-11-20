module Utils.AbsUtils where

import AbsChapel

data ExpType = LeftExp | RightExp | BooleanExp

type Loc = (Int,Int)

getExprDeclPos (ExprDecArray (ArrayInit (POpenBracket (loc,_)) _ _)) = loc
getExprDeclPos (ExprDec exp) = getStartExpPos exp 

getEndExprDeclPos (ExprDecArray (ArrayInit _ _ (PCloseBracket (loc,_)))) = loc
getEndExprDeclPos (ExprDec exp) = getEndExpPos exp 

getFunName (SignNoRet (PIdent (_,identifier)) _) = identifier
getFunName (SignWRet (PIdent (_,identifier)) _ _ _) = identifier

getFunNamePident (SignNoRet r@(PIdent _) _) = r 
getFunNamePident (SignWRet r@(PIdent _) _ _ _) = r


getBodyStartPos (BodyBlock (POpenGraph (locStart,_)) _ _ ) = locStart
getBodyEndPos (BodyBlock _ _ (PCloseGraph (locEnd,_))  ) =locEnd

getStartExpPos exp = case exp of
    Evar (PIdent (loc,_)) -> loc
    Econst (Estring (PString (loc,_))) -> loc
    Econst (Efloat (PDouble (loc,_))) -> loc
    Econst (Echar (PChar (loc,_))) -> loc
    Econst (Eint (PInteger (loc,_))) -> loc
    Econst (ETrue (PTrue (loc,_))) -> loc
    Econst (EFalse (PFalse (loc,_))) -> loc
    EAss e1 _ _ -> getStartExpPos e1
    Elor e1 _ _ -> getStartExpPos e1
    Eland e1 _ _ -> getStartExpPos e1
    Eeq e1 _ _ -> getStartExpPos e1
    Eneq e1 _ _ -> getStartExpPos e1
    Elthen e1 _ _ -> getStartExpPos e1
    Egrthen e1 _ _ -> getStartExpPos e1
    Ele e1 _ _ -> getStartExpPos e1
    Ege e1 _ _ -> getStartExpPos e1
    Eplus e1 _ _ -> getStartExpPos e1
    Eminus e1 _ _ -> getStartExpPos e1
    Etimes e1 _ _ -> getStartExpPos e1
    Ediv e1 _ _ -> getStartExpPos e1
    Emod e1 _ _ -> getStartExpPos e1
    Epreop _ e1 -> getStartExpPos e1
    Earray e1 _ -> getStartExpPos e1
    InnerExp _ e1 _ -> getStartExpPos e1
    EFun (PIdent ((l,c),_)) _ _ _ -> (l,c)

getEndExpPos exp = case exp of
    Evar (PIdent (loc,_)) -> loc
    Econst (Estring (PString (loc,_))) -> loc
    Econst (Efloat (PDouble (loc,_))) -> loc
    Econst (Echar (PChar (loc,_))) -> loc
    Econst (Eint (PInteger (loc,_))) -> loc
    Econst (ETrue (PTrue (loc,_))) -> loc
    Econst (EFalse (PFalse (loc,_))) -> loc
    EAss _ _ e2 -> getEndExpPos e2
    Elor _ _ e2 -> getEndExpPos e2
    Eland _ _ e2 -> getEndExpPos e2
    Eeq _ _ e2 -> getEndExpPos e2
    Eneq _ _ e2 -> getEndExpPos e2
    Elthen _ _ e2 -> getEndExpPos e2
    Egrthen _ _ e2 -> getEndExpPos e2
    Ele _ _ e2 -> getEndExpPos e2
    Ege _ _ e2 -> getEndExpPos e2
    Eplus _ _ e2 -> getEndExpPos e2
    Eminus _ _ e2 -> getEndExpPos e2
    Etimes _ _ e2 -> getEndExpPos e2
    Ediv _ _ e2 -> getEndExpPos e2
    Emod _ _ e2 -> getEndExpPos e2
    Epreop _ e1 -> getEndExpPos e1
    Earray e1 _ -> getEndExpPos e1
    InnerExp _ e1 _ -> getEndExpPos e1
    EFun (PIdent ((l,c),_)) _ _ _ -> (l,c)

getAssignPos assgn = case assgn of 
    AssgnEq (PAssignmEq (loc,_)) ->  loc
    AssgnPlEq (PAssignmPlus (loc,_)) ->  loc

getEpreopPos preop = case preop of
  Indirection (PEtimes (loc,_))  -> loc
  Address (PDef (loc,_))  ->  loc
  Negation (PNeg (loc,_))  ->  loc
  MinusUnary (PEminus (loc,_))  -> loc

getVarPos (Evar (PIdent ((l,c),_))) = (l,c)

getVarId (Evar (PIdent ((l,c),indentifier))) = indentifier

getAssignOpPos op = case op of
  (AssgnEq (PAssignmEq ((l,c),_))) -> (l,c)
  (AssgnPlEq (PAssignmPlus ((l,c),_))) -> (l,c)

getAssignOpTok op = case op of
  (AssgnEq (PAssignmEq ((l,c),t))) -> t
  (AssgnPlEq (PAssignmPlus ((l,c),t))) -> t