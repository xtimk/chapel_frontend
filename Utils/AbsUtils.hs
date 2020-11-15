module Utils.AbsUtils where

import AbsChapel

data ExpType = LeftExp | RightExp | BooleanExp

type Loc = (Int,Int)

getExprDeclPos (ExprDecArray (ArrayInit _ (e:exps) _)) = getExprDeclPos e
getExprDeclPos (ExprDec exp) = getExpPos exp 

getFunName (SignNoRet (PIdent (_,identifier)) _) = identifier
getFunName (SignWRet (PIdent (_,identifier)) _ _ _) = identifier
getFunName (SignWArRet (PIdent (_,identifier)) _ _ _ _) = identifier

getFunNamePident (SignNoRet r@(PIdent _) _) = r 
getFunNamePident (SignWRet r@(PIdent _) _ _ _) = r
getFunNamePident (SignWArRet r@(PIdent _) _ _ _ _) = r


getBodyStartPos (BodyBlock (POpenGraph (locStart,_)) _ _ ) = locStart
getBodyEndPos (BodyBlock _ _ (PCloseGraph (locEnd,_))  ) =locEnd

getExpPos exp = case exp of
    Evar (PIdent (loc,_)) -> loc
    Econst (Estring (PString (loc,_))) -> loc
    Econst (Efloat (PDouble (loc,_))) -> loc
    Econst (Echar (PChar (loc,_))) -> loc
    Econst (Eint (PInteger (loc,_))) -> loc
    Econst (ETrue (PTrue (loc,_))) -> loc
    Econst (EFalse (PFalse (loc,_))) -> loc
    EAss e1 _ _ -> getExpPos e1
    Elor e1 _ _ -> getExpPos e1
    Eland e1 _ _ -> getExpPos e1
    Ebitand e1 _ _ -> getExpPos e1
    Eeq e1 _ _ -> getExpPos e1
    Eneq e1 _ _ -> getExpPos e1
    Elthen e1 _ _ -> getExpPos e1
    Egrthen e1 _ _ -> getExpPos e1
    Ele e1 _ _ -> getExpPos e1
    Ege e1 _ _ -> getExpPos e1
    Eplus e1 _ _ -> getExpPos e1
    Eminus e1 _ _ -> getExpPos e1
    Etimes e1 _ _ -> getExpPos e1
    Ediv e1 _ _ -> getExpPos e1
    Emod e1 _ _ -> getExpPos e1
    Epreop _ e1 -> getExpPos e1
    Earray e1 _ -> getExpPos e1
    InnerExp _ e1 _ -> getExpPos e1
    EFun (PIdent ((l,c),_)) _ _ _ -> (l,c)

getAssignPos assgn = case assgn of 
    AssgnEq (PAssignmEq (loc,_)) ->  loc
    AssgnPlEq (PAssignmPlus (loc,_)) ->  loc

getVarPos (Evar (PIdent ((l,c),_))) = (l,c)

getVarId (Evar (PIdent ((l,c),indentifier))) = indentifier

getAssignOpPos op = case op of
  (AssgnEq (PAssignmEq ((l,c),_))) -> (l,c)
  (AssgnPlEq (PAssignmPlus ((l,c),_))) -> (l,c)

getAssignOpTok op = case op of
  (AssgnEq (PAssignmEq ((l,c),t))) -> t
  (AssgnPlEq (PAssignmPlus ((l,c),t))) -> t