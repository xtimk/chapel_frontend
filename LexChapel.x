-- -*- haskell -*-
-- This Alex file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}
module LexChapel where



import qualified Data.Bits
import Data.Word (Word8)
import Data.Char (ord)
}


$c = [A-Z\192-\221] # [\215]  -- capital isolatin1 letter (215 = \times) FIXME
$s = [a-z\222-\255] # [\247]  -- small   isolatin1 letter (247 = \div  ) FIXME
$l = [$c $s]         -- letter
$d = [0-9]           -- digit
$i = [$l $d _ ']     -- identifier character
$u = [. \n]          -- universal: any character

@rsyms =    -- symbols and non-identifier-like reserved words
   \,

:-

$white+ ;
@rsyms
    { tok (\p s -> PT p (eitherResIdent (TV . share) s)) }
\{
    { tok (\p s -> PT p (eitherResIdent (T_POpenGraph . share) s)) }
\}
    { tok (\p s -> PT p (eitherResIdent (T_PCloseGraph . share) s)) }
\(
    { tok (\p s -> PT p (eitherResIdent (T_POpenParenthesis . share) s)) }
\)
    { tok (\p s -> PT p (eitherResIdent (T_PCloseParenthesis . share) s)) }
\[
    { tok (\p s -> PT p (eitherResIdent (T_POpenBracket . share) s)) }
\]
    { tok (\p s -> PT p (eitherResIdent (T_PCloseBracket . share) s)) }
\;
    { tok (\p s -> PT p (eitherResIdent (T_PSemicolon . share) s)) }
\:
    { tok (\p s -> PT p (eitherResIdent (T_PColon . share) s)) }
\.
    { tok (\p s -> PT p (eitherResIdent (T_PPoint . share) s)) }
i f
    { tok (\p s -> PT p (eitherResIdent (T_PIf . share) s)) }
t h e n
    { tok (\p s -> PT p (eitherResIdent (T_PThen . share) s)) }
e l s e
    { tok (\p s -> PT p (eitherResIdent (T_PElse . share) s)) }
d o
    { tok (\p s -> PT p (eitherResIdent (T_Pdo . share) s)) }
w h i l e
    { tok (\p s -> PT p (eitherResIdent (T_PWhile . share) s)) }
i n t
    { tok (\p s -> PT p (eitherResIdent (T_PIntType . share) s)) }
r e a l
    { tok (\p s -> PT p (eitherResIdent (T_PRealType . share) s)) }
c h a r
    { tok (\p s -> PT p (eitherResIdent (T_PCharType . share) s)) }
b o o l
    { tok (\p s -> PT p (eitherResIdent (T_PBoolType . share) s)) }
s t r i n g
    { tok (\p s -> PT p (eitherResIdent (T_PStringType . share) s)) }
b r e a k
    { tok (\p s -> PT p (eitherResIdent (T_PBreak . share) s)) }
c o n t i n u e
    { tok (\p s -> PT p (eitherResIdent (T_PContinue . share) s)) }
\=
    { tok (\p s -> PT p (eitherResIdent (T_PAssignmEq . share) s)) }
r e f
    { tok (\p s -> PT p (eitherResIdent (T_PRef . share) s)) }
v a r
    { tok (\p s -> PT p (eitherResIdent (T_PVar . share) s)) }
c o n s t
    { tok (\p s -> PT p (eitherResIdent (T_PConst . share) s)) }
p r o c
    { tok (\p s -> PT p (eitherResIdent (T_PProc . share) s)) }
r e t u r n
    { tok (\p s -> PT p (eitherResIdent (T_PReturn . share) s)) }
t r u e
    { tok (\p s -> PT p (eitherResIdent (T_PTrue . share) s)) }
f a l s e
    { tok (\p s -> PT p (eitherResIdent (T_PFalse . share) s)) }
\<
    { tok (\p s -> PT p (eitherResIdent (T_PElthen . share) s)) }
\>
    { tok (\p s -> PT p (eitherResIdent (T_PEgrthen . share) s)) }
\+
    { tok (\p s -> PT p (eitherResIdent (T_PEplus . share) s)) }
\-
    { tok (\p s -> PT p (eitherResIdent (T_PEminus . share) s)) }
\*
    { tok (\p s -> PT p (eitherResIdent (T_PEtimes . share) s)) }
\/
    { tok (\p s -> PT p (eitherResIdent (T_PEdiv . share) s)) }
\%
    { tok (\p s -> PT p (eitherResIdent (T_PEmod . share) s)) }
\&
    { tok (\p s -> PT p (eitherResIdent (T_PDef . share) s)) }
\| \|
    { tok (\p s -> PT p (eitherResIdent (T_PElor . share) s)) }
\& \&
    { tok (\p s -> PT p (eitherResIdent (T_PEland . share) s)) }
\= \=
    { tok (\p s -> PT p (eitherResIdent (T_PEeq . share) s)) }
\! \=
    { tok (\p s -> PT p (eitherResIdent (T_PEneq . share) s)) }
\< \=
    { tok (\p s -> PT p (eitherResIdent (T_PEle . share) s)) }
\> \=
    { tok (\p s -> PT p (eitherResIdent (T_PEge . share) s)) }
\+ \=
    { tok (\p s -> PT p (eitherResIdent (T_PAssignmPlus . share) s)) }
$l ($l | $d | \_ | \')*
    { tok (\p s -> PT p (eitherResIdent (T_PIdent . share) s)) }
\" ($u # [\" \\]| \\ [\" \\ n t]) * \"
    { tok (\p s -> PT p (eitherResIdent (T_PString . share) s)) }
\' ($u # [\' \\]| \\ [\' \\ n t]) \'
    { tok (\p s -> PT p (eitherResIdent (T_PChar . share) s)) }
$d + \. $d + (e \- ? $d +)?
    { tok (\p s -> PT p (eitherResIdent (T_PDouble . share) s)) }
$d +
    { tok (\p s -> PT p (eitherResIdent (T_PInteger . share) s)) }

$l $i*
    { tok (\p s -> PT p (eitherResIdent (TV . share) s)) }





{

tok :: (Posn -> String -> Token) -> (Posn -> String -> Token)
tok f p s = f p s

share :: String -> String
share = id

data Tok =
   TS !String !Int    -- reserved words and symbols
 | TL !String         -- string literals
 | TI !String         -- integer literals
 | TV !String         -- identifiers
 | TD !String         -- double precision float literals
 | TC !String         -- character literals
 | T_POpenGraph !String
 | T_PCloseGraph !String
 | T_POpenParenthesis !String
 | T_PCloseParenthesis !String
 | T_POpenBracket !String
 | T_PCloseBracket !String
 | T_PSemicolon !String
 | T_PColon !String
 | T_PPoint !String
 | T_PIf !String
 | T_PThen !String
 | T_PElse !String
 | T_Pdo !String
 | T_PWhile !String
 | T_PIntType !String
 | T_PRealType !String
 | T_PCharType !String
 | T_PBoolType !String
 | T_PStringType !String
 | T_PBreak !String
 | T_PContinue !String
 | T_PAssignmEq !String
 | T_PRef !String
 | T_PVar !String
 | T_PConst !String
 | T_PProc !String
 | T_PReturn !String
 | T_PTrue !String
 | T_PFalse !String
 | T_PElthen !String
 | T_PEgrthen !String
 | T_PEplus !String
 | T_PEminus !String
 | T_PEtimes !String
 | T_PEdiv !String
 | T_PEmod !String
 | T_PDef !String
 | T_PElor !String
 | T_PEland !String
 | T_PEeq !String
 | T_PEneq !String
 | T_PEle !String
 | T_PEge !String
 | T_PAssignmPlus !String
 | T_PIdent !String
 | T_PString !String
 | T_PChar !String
 | T_PDouble !String
 | T_PInteger !String

 deriving (Eq,Show,Ord)

data Token =
   PT  Posn Tok
 | Err Posn
  deriving (Eq,Show,Ord)

printPosn :: Posn -> String
printPosn (Pn _ l c) = "line " ++ show l ++ ", column " ++ show c

tokenPos :: [Token] -> String
tokenPos (t:_) = printPosn (tokenPosn t)
tokenPos [] = "end of file"

tokenPosn :: Token -> Posn
tokenPosn (PT p _) = p
tokenPosn (Err p) = p

tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

mkPosToken :: Token -> ((Int, Int), String)
mkPosToken t@(PT p _) = (posLineCol p, prToken t)

prToken :: Token -> String
prToken t = case t of
  PT _ (TS s _) -> s
  PT _ (TL s)   -> show s
  PT _ (TI s)   -> s
  PT _ (TV s)   -> s
  PT _ (TD s)   -> s
  PT _ (TC s)   -> s
  Err _         -> "#error"
  PT _ (T_POpenGraph s) -> s
  PT _ (T_PCloseGraph s) -> s
  PT _ (T_POpenParenthesis s) -> s
  PT _ (T_PCloseParenthesis s) -> s
  PT _ (T_POpenBracket s) -> s
  PT _ (T_PCloseBracket s) -> s
  PT _ (T_PSemicolon s) -> s
  PT _ (T_PColon s) -> s
  PT _ (T_PPoint s) -> s
  PT _ (T_PIf s) -> s
  PT _ (T_PThen s) -> s
  PT _ (T_PElse s) -> s
  PT _ (T_Pdo s) -> s
  PT _ (T_PWhile s) -> s
  PT _ (T_PIntType s) -> s
  PT _ (T_PRealType s) -> s
  PT _ (T_PCharType s) -> s
  PT _ (T_PBoolType s) -> s
  PT _ (T_PStringType s) -> s
  PT _ (T_PBreak s) -> s
  PT _ (T_PContinue s) -> s
  PT _ (T_PAssignmEq s) -> s
  PT _ (T_PRef s) -> s
  PT _ (T_PVar s) -> s
  PT _ (T_PConst s) -> s
  PT _ (T_PProc s) -> s
  PT _ (T_PReturn s) -> s
  PT _ (T_PTrue s) -> s
  PT _ (T_PFalse s) -> s
  PT _ (T_PElthen s) -> s
  PT _ (T_PEgrthen s) -> s
  PT _ (T_PEplus s) -> s
  PT _ (T_PEminus s) -> s
  PT _ (T_PEtimes s) -> s
  PT _ (T_PEdiv s) -> s
  PT _ (T_PEmod s) -> s
  PT _ (T_PDef s) -> s
  PT _ (T_PElor s) -> s
  PT _ (T_PEland s) -> s
  PT _ (T_PEeq s) -> s
  PT _ (T_PEneq s) -> s
  PT _ (T_PEle s) -> s
  PT _ (T_PEge s) -> s
  PT _ (T_PAssignmPlus s) -> s
  PT _ (T_PIdent s) -> s
  PT _ (T_PString s) -> s
  PT _ (T_PChar s) -> s
  PT _ (T_PDouble s) -> s
  PT _ (T_PInteger s) -> s


data BTree = N | B String Tok BTree BTree deriving (Show)

eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) | s < a  = treeFind left
                              | s > a  = treeFind right
                              | s == a = t

resWords :: BTree
resWords = b "," 1 N N
   where b s n = let bs = id s
                  in B bs (TS bs n)

unescapeInitTail :: String -> String
unescapeInitTail = id . unesc . tail . id where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '\\':'r':cs  -> '\r' : unesc cs
    '\\':'f':cs  -> '\f' : unesc cs
    '"':[]    -> []
    c:cs      -> c : unesc cs
    _         -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
      deriving (Eq, Show,Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos (take len str) : (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case  s of
    []  -> Nothing
    (c:s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
}
