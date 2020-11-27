{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParChapel where
import AbsChapel
import LexChapel
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 (POpenGraph)
happyIn4 :: (POpenGraph) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 (PCloseGraph)
happyIn5 :: (PCloseGraph) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 (POpenParenthesis)
happyIn6 :: (POpenParenthesis) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 (PCloseParenthesis)
happyIn7 :: (PCloseParenthesis) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 (POpenBracket)
happyIn8 :: (POpenBracket) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 (PCloseBracket)
happyIn9 :: (PCloseBracket) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 (PSemicolon)
happyIn10 :: (PSemicolon) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 (PColon)
happyIn11 :: (PColon) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (PPoint)
happyIn12 :: (PPoint) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 (PIf)
happyIn13 :: (PIf) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 (PThen)
happyIn14 :: (PThen) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 (PElse)
happyIn15 :: (PElse) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 (Pdo)
happyIn16 :: (Pdo) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 (PWhile)
happyIn17 :: (PWhile) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 (PIntType)
happyIn18 :: (PIntType) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 (PRealType)
happyIn19 :: (PRealType) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 (PCharType)
happyIn20 :: (PCharType) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 (PBoolType)
happyIn21 :: (PBoolType) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (PStringType)
happyIn22 :: (PStringType) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 (PBreak)
happyIn23 :: (PBreak) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 (PContinue)
happyIn24 :: (PContinue) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 (PAssignmEq)
happyIn25 :: (PAssignmEq) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 (PRef)
happyIn26 :: (PRef) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 (PVar)
happyIn27 :: (PVar) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 (PProc)
happyIn28 :: (PProc) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 (PReturn)
happyIn29 :: (PReturn) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 (PTrue)
happyIn30 :: (PTrue) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 (PFalse)
happyIn31 :: (PFalse) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 (PEpow)
happyIn32 :: (PEpow) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 (PElthen)
happyIn33 :: (PElthen) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 (PEgrthen)
happyIn34 :: (PEgrthen) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 (PEplus)
happyIn35 :: (PEplus) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 (PEminus)
happyIn36 :: (PEminus) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 (PEtimes)
happyIn37 :: (PEtimes) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 (PEdiv)
happyIn38 :: (PEdiv) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 (PEmod)
happyIn39 :: (PEmod) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 (PDef)
happyIn40 :: (PDef) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 (PNeg)
happyIn41 :: (PNeg) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 (PElor)
happyIn42 :: (PElor) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 (PEland)
happyIn43 :: (PEland) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 (PEeq)
happyIn44 :: (PEeq) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 (PEneq)
happyIn45 :: (PEneq) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 (PEle)
happyIn46 :: (PEle) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 (PEge)
happyIn47 :: (PEge) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 (PAssignmPlus)
happyIn48 :: (PAssignmPlus) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 (PIdent)
happyIn49 :: (PIdent) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 (PString)
happyIn50 :: (PString) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 (PChar)
happyIn51 :: (PChar) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 (PDouble)
happyIn52 :: (PDouble) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 (PInteger)
happyIn53 :: (PInteger) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 (Program)
happyIn54 :: (Program) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 (Module)
happyIn55 :: (Module) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 ([Ext])
happyIn56 :: ([Ext]) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 (Ext)
happyIn57 :: (Ext) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 (Declaration)
happyIn58 :: (Declaration) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 ([DeclList])
happyIn59 :: ([DeclList]) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
newtype HappyWrap60 = HappyWrap60 (DeclList)
happyIn60 :: (DeclList) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap60 x)
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> HappyWrap60
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
newtype HappyWrap61 = HappyWrap61 (TypeSpec)
happyIn61 :: (TypeSpec) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap61 x)
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> HappyWrap61
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
newtype HappyWrap62 = HappyWrap62 (ExprDecl)
happyIn62 :: (ExprDecl) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap62 x)
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> HappyWrap62
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
newtype HappyWrap63 = HappyWrap63 (ArInit)
happyIn63 :: (ArInit) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap63 x)
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> HappyWrap63
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
newtype HappyWrap64 = HappyWrap64 ([ExprDecl])
happyIn64 :: ([ExprDecl]) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap64 x)
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> HappyWrap64
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
newtype HappyWrap65 = HappyWrap65 (ArDecl)
happyIn65 :: (ArDecl) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap65 x)
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> HappyWrap65
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
newtype HappyWrap66 = HappyWrap66 (ArDim)
happyIn66 :: (ArDim) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap66 x)
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> HappyWrap66
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
newtype HappyWrap67 = HappyWrap67 ([ArDim])
happyIn67 :: ([ArDim]) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap67 x)
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> HappyWrap67
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
newtype HappyWrap68 = HappyWrap68 (ArBound)
happyIn68 :: (ArBound) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap68 x)
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> HappyWrap68
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
newtype HappyWrap69 = HappyWrap69 ([PIdent])
happyIn69 :: ([PIdent]) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap69 x)
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> HappyWrap69
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
newtype HappyWrap70 = HappyWrap70 (DecMode)
happyIn70 :: (DecMode) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap70 x)
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> HappyWrap70
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
newtype HappyWrap71 = HappyWrap71 (Function)
happyIn71 :: (Function) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap71 x)
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> HappyWrap71
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
newtype HappyWrap72 = HappyWrap72 (Signature)
happyIn72 :: (Signature) -> (HappyAbsSyn )
happyIn72 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap72 x)
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> HappyWrap72
happyOut72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut72 #-}
newtype HappyWrap73 = HappyWrap73 (FunctionParams)
happyIn73 :: (FunctionParams) -> (HappyAbsSyn )
happyIn73 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap73 x)
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> HappyWrap73
happyOut73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut73 #-}
newtype HappyWrap74 = HappyWrap74 ([Param])
happyIn74 :: ([Param]) -> (HappyAbsSyn )
happyIn74 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap74 x)
{-# INLINE happyIn74 #-}
happyOut74 :: (HappyAbsSyn ) -> HappyWrap74
happyOut74 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut74 #-}
newtype HappyWrap75 = HappyWrap75 (Param)
happyIn75 :: (Param) -> (HappyAbsSyn )
happyIn75 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap75 x)
{-# INLINE happyIn75 #-}
happyOut75 :: (HappyAbsSyn ) -> HappyWrap75
happyOut75 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut75 #-}
newtype HappyWrap76 = HappyWrap76 ([PassedParam])
happyIn76 :: ([PassedParam]) -> (HappyAbsSyn )
happyIn76 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap76 x)
{-# INLINE happyIn76 #-}
happyOut76 :: (HappyAbsSyn ) -> HappyWrap76
happyOut76 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut76 #-}
newtype HappyWrap77 = HappyWrap77 (PassedParam)
happyIn77 :: (PassedParam) -> (HappyAbsSyn )
happyIn77 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap77 x)
{-# INLINE happyIn77 #-}
happyOut77 :: (HappyAbsSyn ) -> HappyWrap77
happyOut77 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut77 #-}
newtype HappyWrap78 = HappyWrap78 (Body)
happyIn78 :: (Body) -> (HappyAbsSyn )
happyIn78 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap78 x)
{-# INLINE happyIn78 #-}
happyOut78 :: (HappyAbsSyn ) -> HappyWrap78
happyOut78 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut78 #-}
newtype HappyWrap79 = HappyWrap79 ([BodyStatement])
happyIn79 :: ([BodyStatement]) -> (HappyAbsSyn )
happyIn79 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap79 x)
{-# INLINE happyIn79 #-}
happyOut79 :: (HappyAbsSyn ) -> HappyWrap79
happyOut79 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut79 #-}
newtype HappyWrap80 = HappyWrap80 (BodyStatement)
happyIn80 :: (BodyStatement) -> (HappyAbsSyn )
happyIn80 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap80 x)
{-# INLINE happyIn80 #-}
happyOut80 :: (HappyAbsSyn ) -> HappyWrap80
happyOut80 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut80 #-}
newtype HappyWrap81 = HappyWrap81 (Statement)
happyIn81 :: (Statement) -> (HappyAbsSyn )
happyIn81 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap81 x)
{-# INLINE happyIn81 #-}
happyOut81 :: (HappyAbsSyn ) -> HappyWrap81
happyOut81 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut81 #-}
newtype HappyWrap82 = HappyWrap82 (Guard)
happyIn82 :: (Guard) -> (HappyAbsSyn )
happyIn82 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap82 x)
{-# INLINE happyIn82 #-}
happyOut82 :: (HappyAbsSyn ) -> HappyWrap82
happyOut82 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut82 #-}
newtype HappyWrap83 = HappyWrap83 (Type)
happyIn83 :: (Type) -> (HappyAbsSyn )
happyIn83 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap83 x)
{-# INLINE happyIn83 #-}
happyOut83 :: (HappyAbsSyn ) -> HappyWrap83
happyOut83 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut83 #-}
newtype HappyWrap84 = HappyWrap84 (AssgnmOp)
happyIn84 :: (AssgnmOp) -> (HappyAbsSyn )
happyIn84 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap84 x)
{-# INLINE happyIn84 #-}
happyOut84 :: (HappyAbsSyn ) -> HappyWrap84
happyOut84 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut84 #-}
newtype HappyWrap85 = HappyWrap85 (Mode)
happyIn85 :: (Mode) -> (HappyAbsSyn )
happyIn85 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap85 x)
{-# INLINE happyIn85 #-}
happyOut85 :: (HappyAbsSyn ) -> HappyWrap85
happyOut85 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut85 #-}
newtype HappyWrap86 = HappyWrap86 (Exp)
happyIn86 :: (Exp) -> (HappyAbsSyn )
happyIn86 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap86 x)
{-# INLINE happyIn86 #-}
happyOut86 :: (HappyAbsSyn ) -> HappyWrap86
happyOut86 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut86 #-}
newtype HappyWrap87 = HappyWrap87 (Exp)
happyIn87 :: (Exp) -> (HappyAbsSyn )
happyIn87 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap87 x)
{-# INLINE happyIn87 #-}
happyOut87 :: (HappyAbsSyn ) -> HappyWrap87
happyOut87 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut87 #-}
newtype HappyWrap88 = HappyWrap88 (Exp)
happyIn88 :: (Exp) -> (HappyAbsSyn )
happyIn88 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap88 x)
{-# INLINE happyIn88 #-}
happyOut88 :: (HappyAbsSyn ) -> HappyWrap88
happyOut88 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut88 #-}
newtype HappyWrap89 = HappyWrap89 (Exp)
happyIn89 :: (Exp) -> (HappyAbsSyn )
happyIn89 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap89 x)
{-# INLINE happyIn89 #-}
happyOut89 :: (HappyAbsSyn ) -> HappyWrap89
happyOut89 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut89 #-}
newtype HappyWrap90 = HappyWrap90 (Exp)
happyIn90 :: (Exp) -> (HappyAbsSyn )
happyIn90 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap90 x)
{-# INLINE happyIn90 #-}
happyOut90 :: (HappyAbsSyn ) -> HappyWrap90
happyOut90 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut90 #-}
newtype HappyWrap91 = HappyWrap91 (Exp)
happyIn91 :: (Exp) -> (HappyAbsSyn )
happyIn91 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap91 x)
{-# INLINE happyIn91 #-}
happyOut91 :: (HappyAbsSyn ) -> HappyWrap91
happyOut91 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut91 #-}
newtype HappyWrap92 = HappyWrap92 (Exp)
happyIn92 :: (Exp) -> (HappyAbsSyn )
happyIn92 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap92 x)
{-# INLINE happyIn92 #-}
happyOut92 :: (HappyAbsSyn ) -> HappyWrap92
happyOut92 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut92 #-}
newtype HappyWrap93 = HappyWrap93 (Exp)
happyIn93 :: (Exp) -> (HappyAbsSyn )
happyIn93 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap93 x)
{-# INLINE happyIn93 #-}
happyOut93 :: (HappyAbsSyn ) -> HappyWrap93
happyOut93 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut93 #-}
newtype HappyWrap94 = HappyWrap94 (Exp)
happyIn94 :: (Exp) -> (HappyAbsSyn )
happyIn94 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap94 x)
{-# INLINE happyIn94 #-}
happyOut94 :: (HappyAbsSyn ) -> HappyWrap94
happyOut94 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut94 #-}
newtype HappyWrap95 = HappyWrap95 (Exp)
happyIn95 :: (Exp) -> (HappyAbsSyn )
happyIn95 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap95 x)
{-# INLINE happyIn95 #-}
happyOut95 :: (HappyAbsSyn ) -> HappyWrap95
happyOut95 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut95 #-}
newtype HappyWrap96 = HappyWrap96 (Exp)
happyIn96 :: (Exp) -> (HappyAbsSyn )
happyIn96 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap96 x)
{-# INLINE happyIn96 #-}
happyOut96 :: (HappyAbsSyn ) -> HappyWrap96
happyOut96 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut96 #-}
newtype HappyWrap97 = HappyWrap97 (UnaryOperator)
happyIn97 :: (UnaryOperator) -> (HappyAbsSyn )
happyIn97 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap97 x)
{-# INLINE happyIn97 #-}
happyOut97 :: (HappyAbsSyn ) -> HappyWrap97
happyOut97 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut97 #-}
newtype HappyWrap98 = HappyWrap98 (Constant)
happyIn98 :: (Constant) -> (HappyAbsSyn )
happyIn98 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap98 x)
{-# INLINE happyIn98 #-}
happyOut98 :: (HappyAbsSyn ) -> HappyWrap98
happyOut98 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut98 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x40\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xe0\x03\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x04\x00\x18\x66\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x00\x80\x61\x06\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x10\x00\x60\x98\x01\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\x00\x86\x19\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xe0\x03\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3e\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\xf8\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x03\x19\xcc\x87\x19\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x04\x00\x18\x66\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x04\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xe0\x03\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x04\x00\x18\x66\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\x00\x06\x00\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x60\x98\x01\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x04\x00\x18\x66\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\x00\x86\x19\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x04\x00\x18\x66\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\x00\x86\x19\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x60\x98\x01\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x04\x00\x18\x66\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\x00\x86\x19\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x00\x80\x61\x06\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x60\x98\x01\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x04\x00\x18\x66\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x60\x98\x01\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\x00\x86\x19\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x60\x98\x01\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x04\x80\x18\x66\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\x00\x86\x19\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x10\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x01\x00\x86\x19\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x00\x80\x01\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x40\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x60\x98\x01\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x3e\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x01\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x01\x00\x06\x00\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\x20\x86\x19\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x00\x80\x01\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","POpenGraph","PCloseGraph","POpenParenthesis","PCloseParenthesis","POpenBracket","PCloseBracket","PSemicolon","PColon","PPoint","PIf","PThen","PElse","Pdo","PWhile","PIntType","PRealType","PCharType","PBoolType","PStringType","PBreak","PContinue","PAssignmEq","PRef","PVar","PProc","PReturn","PTrue","PFalse","PEpow","PElthen","PEgrthen","PEplus","PEminus","PEtimes","PEdiv","PEmod","PDef","PNeg","PElor","PEland","PEeq","PEneq","PEle","PEge","PAssignmPlus","PIdent","PString","PChar","PDouble","PInteger","Program","Module","ListExt","Ext","Declaration","ListDeclList","DeclList","TypeSpec","ExprDecl","ArInit","ListExprDecl","ArDecl","ArDim","ListArDim","ArBound","ListPIdent","DecMode","Function","Signature","FunctionParams","ListParam","Param","ListPassedParam","PassedParam","Body","ListBodyStatement","BodyStatement","Statement","Guard","Type","AssgnmOp","Mode","Exp","Exp1","Exp2","Exp3","Exp4","Exp5","Exp6","Exp7","Exp8","Exp9","Exp10","UnaryOperator","Constant","','","L_POpenGraph","L_PCloseGraph","L_POpenParenthesis","L_PCloseParenthesis","L_POpenBracket","L_PCloseBracket","L_PSemicolon","L_PColon","L_PPoint","L_PIf","L_PThen","L_PElse","L_Pdo","L_PWhile","L_PIntType","L_PRealType","L_PCharType","L_PBoolType","L_PStringType","L_PBreak","L_PContinue","L_PAssignmEq","L_PRef","L_PVar","L_PProc","L_PReturn","L_PTrue","L_PFalse","L_PEpow","L_PElthen","L_PEgrthen","L_PEplus","L_PEminus","L_PEtimes","L_PEdiv","L_PEmod","L_PDef","L_PNeg","L_PElor","L_PEland","L_PEeq","L_PEneq","L_PEle","L_PEge","L_PAssignmPlus","L_PIdent","L_PString","L_PChar","L_PDouble","L_PInteger","%eof"]
        bit_start = st * 150
        bit_end = (st + 1) * 150
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..149]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x03\x00\x00\x00\xdf\xff\x00\x00\x13\x00\x00\x00\xe8\xff\x00\x00\x00\x00\xe8\xff\x00\x00\x00\x00\x00\x00\x1c\x00\x31\x00\x47\x00\xdc\x00\x00\x00\x5e\x00\x79\x00\x00\x00\x00\x00\xf0\xff\x76\x00\x00\x00\x5b\x01\x00\x00\x00\x00\xb4\x05\x00\x00\x00\x00\x00\x00\x52\x00\x00\x00\x00\x00\x52\x00\x00\x00\x00\x00\x59\x06\xb4\x05\x81\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x58\x00\x75\x00\x7f\x00\xe1\xff\x22\x01\x51\x00\xed\xff\x00\x00\x91\x00\xc6\x00\x00\x00\x59\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb7\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x01\x58\x00\x81\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x01\x00\x00\xc8\x00\xca\x00\xd4\x00\xb9\x00\x00\x00\x9a\x05\x00\x00\xe6\x00\xee\x00\xf6\x00\xf8\x00\xf8\x00\xd1\x05\x00\x00\xf8\x00\x00\x00\x00\x00\x00\x00\xed\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\x00\xf1\xff\x00\x00\x00\x00\x5b\x01\x00\x00\x00\x00\xeb\x05\x00\x00\x00\x00\x08\x01\x07\x01\x0a\x01\x00\x00\x00\x00\x00\x00\x68\x06\x00\x00\x59\x06\x59\x06\x59\x06\x00\x00\x00\x00\x59\x06\x59\x06\x00\x00\x59\x06\x59\x06\x59\x06\x59\x06\x00\x00\x00\x00\x00\x00\x00\x00\x59\x06\x59\x06\x00\x00\x00\x00\x59\x06\x00\x00\x59\x06\x00\x00\x59\x06\x08\x06\x59\x06\x0c\x01\x21\x01\x1e\x01\x99\x00\x00\x00\x00\x00\x00\x00\x22\x06\x68\x06\x00\x00\x99\x00\x2d\x01\x32\x01\x59\x06\x8a\x00\x1b\x01\x1c\x01\x82\x00\x22\x01\x22\x01\xa8\x00\xa8\x00\xa8\x00\xa8\x00\xed\xff\xed\xff\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x01\x00\x00\x00\x00\xb7\x01\x00\x00\x00\x00\x00\x00\x5b\x01\x00\x00\x00\x00\x00\x00\xed\x00\x00\x00\x00\x00\x44\x01\x3b\x01\x3d\x01\x29\x05\x47\x01\x00\x00\x00\x00\x00\x00\x00\x00\xb7\x01\x8a\x00\x3f\x06\x00\x00\x00\x00\x3f\x01\x00\x00\x68\x06\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x01\x4b\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xbb\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbc\x00\x00\x00\xa5\x00\x00\x00\x00\x00\x43\x01\x00\x00\x00\x00\x00\x00\x00\x00\x59\x01\x00\x00\xa4\x00\x00\x00\x02\x00\x34\x00\x17\x01\x00\x00\x33\x00\x5d\x01\x00\x00\x52\x03\x00\x00\x00\x00\x5c\x01\x00\x00\x00\x00\x00\x00\x61\x01\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x03\x02\x68\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\x4c\x01\x3c\x01\xe3\x00\x65\x01\xf7\x00\xd6\x00\x00\x00\x57\x01\x42\x00\x00\x00\x89\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb1\x03\x7b\x00\x58\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x04\x00\x00\x6e\x01\x71\x01\x00\x00\xba\x00\x00\x00\x01\x00\x00\x00\x27\x00\x40\x00\x28\x00\x72\x01\x79\x01\xbe\x01\x00\x00\x7f\x01\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x01\xe0\x00\x00\x00\x00\x00\x40\x05\x00\x00\x00\x00\xa1\x01\x00\x00\x00\x00\x00\x00\x9b\x01\x7e\x01\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\xa3\x04\xd5\x04\xef\x04\x00\x00\x00\x00\x3d\x04\x57\x04\x00\x00\x7f\x03\x99\x03\xde\x03\xf8\x03\x00\x00\x00\x00\x00\x00\x00\x00\x20\x03\x3a\x03\x00\x00\x00\x00\xdb\x02\x00\x00\xc1\x02\x00\x00\x7c\x02\x85\x00\x1d\x02\x97\x01\x00\x00\xa3\x01\x20\x00\x00\x00\x00\x00\x00\x00\xe2\x00\x09\x05\x00\x00\x4c\x00\xa6\x01\x00\x00\x62\x02\x9c\x00\x80\x01\x96\x01\x00\x01\x65\x01\x65\x01\x0b\x01\x0b\x01\x0b\x01\x0b\x01\xd6\x00\xd6\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\x01\x00\x00\x00\x00\x64\x05\x00\x00\x00\x00\x00\x00\x62\x05\x00\x00\x00\x00\x00\x00\x67\x00\x00\x00\x00\x00\x41\x00\xb2\x01\xb9\x01\x36\x00\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbb\x04\x9c\x00\xff\x00\x00\x00\x00\x00\xbd\x01\x00\x00\x0f\x05\x00\x00\x00\x00\x00\x00\x00\x00\xbf\x01\x45\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xca\xff\x00\x00\xfe\xff\x00\x00\xcc\xff\xcb\xff\xb0\xff\x00\x00\xc9\xff\xc8\xff\x00\x00\xc7\xff\xe7\xff\xe6\xff\xb2\xff\x00\x00\xc5\xff\x00\x00\xd1\xff\x00\x00\x00\x00\xa0\xff\xaf\xff\xab\xff\xae\xff\xfc\xff\x00\x00\x8a\xff\x89\xff\x00\x00\xf7\xff\xe9\xff\xd2\xff\x00\x00\xc6\xff\xf8\xff\x00\x00\xb1\xff\xc4\xff\x00\x00\x00\x00\x00\x00\x5f\xff\x5e\xff\x66\xff\x64\xff\x65\xff\x67\xff\x69\xff\x63\xff\x61\xff\x62\xff\x60\xff\xc1\xff\xbe\xff\xbd\xff\x86\xff\x84\xff\x82\xff\x7f\xff\x7a\xff\x77\xff\x73\xff\x71\xff\x6e\xff\x6c\xff\x00\x00\x68\xff\xfa\xff\xf5\xff\xe4\xff\xe3\xff\xde\xff\xdd\xff\xda\xff\xd9\xff\xd0\xff\xcf\xff\xce\xff\xcd\xff\x00\x00\x90\xff\x8f\xff\x8e\xff\x8c\xff\x8d\xff\x00\x00\xc3\xff\x00\x00\xc0\xff\xf0\xff\xef\xff\xee\xff\xed\xff\xec\xff\x00\x00\x88\xff\x00\x00\x00\x00\xaa\xff\x00\x00\xe8\xff\x00\x00\xa1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9c\xff\x00\x00\x9b\xff\x9f\xff\x9e\xff\x00\x00\xfd\xff\xf2\xff\xf1\xff\xeb\xff\xea\xff\xe5\xff\x00\x00\xab\xff\xac\xff\xfb\xff\x00\x00\xad\xff\xbf\xff\x00\x00\x8b\xff\xb4\xff\xb6\xff\x00\x00\xb7\xff\xb3\xff\x72\xff\x6f\xff\x00\x00\xe2\xff\x00\x00\x00\x00\x00\x00\xdc\xff\xdb\xff\x00\x00\x00\x00\xdf\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\xe0\xff\xd4\xff\xd3\xff\x00\x00\x00\x00\xd6\xff\xd5\xff\x00\x00\xd7\xff\x00\x00\xd8\xff\x00\x00\xa6\xff\x00\x00\x00\x00\xbb\xff\x00\x00\x00\x00\x6b\xff\xbc\xff\xf9\xff\x00\x00\x00\x00\xf4\xff\x00\x00\x00\x00\xa5\xff\x00\x00\xa3\xff\x87\xff\x85\xff\x83\xff\x80\xff\x81\xff\x7b\xff\x7c\xff\x7d\xff\x7e\xff\x78\xff\x79\xff\x74\xff\x75\xff\x76\xff\x70\xff\x00\x00\xf6\xff\xb9\xff\x00\x00\xc2\xff\xa8\xff\xa9\xff\x00\x00\x92\xff\x9d\xff\x95\xff\x00\x00\x94\xff\x93\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\xff\x96\xff\xa7\xff\xb5\xff\x00\x00\xa2\xff\xa6\xff\x6a\xff\x91\xff\x00\x00\xba\xff\x00\x00\xf3\xff\xa4\xff\xb8\xff\x9a\xff\x98\xff\x00\x00\x6d\xff\x97\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x00\x00\x01\x00\x02\x00\x02\x00\x02\x00\x02\x00\x15\x00\x18\x00\x18\x00\x09\x00\x2a\x00\x2b\x00\x0c\x00\x0d\x00\x02\x00\x23\x00\x24\x00\x25\x00\x34\x00\x13\x00\x14\x00\x09\x00\x2f\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x01\x00\x2c\x00\x2f\x00\x2f\x00\x20\x00\x21\x00\x03\x00\x06\x00\x24\x00\x25\x00\x1a\x00\x1b\x00\x02\x00\x02\x00\x02\x00\x19\x00\x1a\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x15\x00\x00\x00\x15\x00\x00\x00\x36\x00\x02\x00\x08\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x09\x00\x00\x00\x00\x00\x50\x00\x42\x00\x43\x00\x00\x00\x04\x00\x45\x00\x01\x00\x16\x00\x2c\x00\x4a\x00\x2c\x00\x4c\x00\x4d\x00\x03\x00\x1a\x00\x1b\x00\x4e\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x2d\x00\x15\x00\x04\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x5b\x00\x5c\x00\x02\x00\x5e\x00\x04\x00\x06\x00\x50\x00\x17\x00\x50\x00\x09\x00\x21\x00\x22\x00\x41\x00\x4e\x00\x4e\x00\x4e\x00\x2c\x00\x46\x00\x47\x00\x02\x00\x15\x00\x3b\x00\x4a\x00\x09\x00\x4a\x00\x2f\x00\x1a\x00\x1b\x00\x51\x00\x04\x00\x2e\x00\x02\x00\x20\x00\x21\x00\x4a\x00\x4a\x00\x24\x00\x25\x00\x09\x00\x4a\x00\x15\x00\x5b\x00\x5c\x00\x2c\x00\x5e\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x2d\x00\x16\x00\x50\x00\x28\x00\x05\x00\x1a\x00\x1b\x00\x17\x00\x3a\x00\x3b\x00\x3c\x00\x20\x00\x21\x00\x2c\x00\x29\x00\x24\x00\x25\x00\x07\x00\x2a\x00\x2b\x00\x41\x00\x1e\x00\x17\x00\x15\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x50\x00\x2e\x00\x15\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x2e\x00\x2c\x00\x21\x00\x22\x00\x50\x00\x06\x00\x48\x00\x49\x00\x05\x00\x2c\x00\x09\x00\x2d\x00\x17\x00\x18\x00\x01\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x02\x00\x09\x00\x04\x00\x2d\x00\x2f\x00\x44\x00\x04\x00\x09\x00\x50\x00\x32\x00\x33\x00\x34\x00\x02\x00\x35\x00\x36\x00\x17\x00\x50\x00\x08\x00\x16\x00\x21\x00\x22\x00\x23\x00\x04\x00\x41\x00\x1a\x00\x1b\x00\x42\x00\x43\x00\x08\x00\x02\x00\x20\x00\x21\x00\x17\x00\x09\x00\x24\x00\x25\x00\x09\x00\x01\x00\x2e\x00\x28\x00\x29\x00\x2d\x00\x07\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x0a\x00\x16\x00\x1f\x00\x20\x00\x0c\x00\x1a\x00\x1b\x00\x2e\x00\x3a\x00\x3b\x00\x3c\x00\x20\x00\x21\x00\x41\x00\x01\x00\x24\x00\x25\x00\x07\x00\x46\x00\x47\x00\x28\x00\x29\x00\x1f\x00\x20\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x51\x00\x05\x00\x01\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x1f\x00\x20\x00\x28\x00\x0a\x00\x29\x00\x02\x00\x48\x00\x49\x00\x0c\x00\x0f\x00\x04\x00\x0d\x00\x02\x00\x2c\x00\x2d\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x02\x00\x06\x00\x04\x00\x06\x00\x4b\x00\x27\x00\x07\x00\x09\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x2d\x00\x02\x00\x26\x00\x1c\x00\x03\x00\x07\x00\x1a\x00\x1b\x00\x06\x00\x21\x00\x37\x00\x38\x00\x20\x00\x21\x00\x23\x00\x06\x00\x24\x00\x25\x00\x1d\x00\x1e\x00\x41\x00\x06\x00\x08\x00\x1a\x00\x1b\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x2d\x00\x2a\x00\x2b\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x3a\x00\x3b\x00\x37\x00\x38\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x07\x00\x05\x00\x0a\x00\x41\x00\x02\x00\x23\x00\x04\x00\x26\x00\x4f\x00\x05\x00\x03\x00\x09\x00\x3e\x00\x3f\x00\x40\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x1a\x00\x1b\x00\x27\x00\x08\x00\x0d\x00\x02\x00\x20\x00\x21\x00\x0a\x00\x06\x00\x24\x00\x25\x00\x09\x00\x0b\x00\xff\xff\x0b\x00\x5e\x00\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x1c\x00\x1d\x00\xff\xff\xff\xff\xff\xff\x1a\x00\x1b\x00\xff\xff\x3a\x00\x3b\x00\xff\xff\x20\x00\x21\x00\xff\xff\xff\xff\x24\x00\x25\x00\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\xff\xff\xff\xff\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\x09\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\xff\xff\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\xff\xff\xff\xff\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\xff\xff\xff\xff\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\x09\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\xff\xff\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\xff\xff\xff\xff\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\xff\xff\xff\xff\xff\xff\xff\xff\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\x09\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\xff\xff\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\xff\xff\xff\xff\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\x09\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\x04\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\xff\xff\x24\x00\x25\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\xff\xff\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\xff\xff\xff\xff\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\xff\xff\xff\xff\x39\x00\xff\xff\xff\xff\xff\xff\x3d\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\x4f\x00\x09\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\x04\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\xff\xff\x24\x00\x25\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\xff\xff\xff\xff\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\xff\xff\xff\xff\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\xff\xff\xff\xff\x39\x00\xff\xff\xff\xff\xff\xff\x3d\x00\xff\xff\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\x4f\x00\x09\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\x04\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\xff\xff\x24\x00\x25\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\xff\xff\xff\xff\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\xff\xff\xff\xff\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\xff\xff\xff\xff\x39\x00\xff\xff\xff\xff\xff\xff\x3d\x00\xff\xff\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\x4f\x00\x09\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\xff\xff\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\xff\xff\xff\xff\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\x09\x00\x24\x00\x25\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\xff\xff\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\xff\xff\xff\xff\xff\xff\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\x09\x00\x24\x00\x25\x00\x40\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\x02\x00\x09\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\x09\x00\x5e\x00\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1a\x00\x1b\x00\x02\x00\xff\xff\x04\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x0b\x00\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\xff\xff\x04\x00\x1c\x00\x1d\x00\xff\xff\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\xff\xff\x5b\x00\x5c\x00\x04\x00\x5e\x00\xff\xff\xff\xff\x5b\x00\x5c\x00\xff\xff\x5e\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\x39\x00\xff\xff\xff\xff\xff\xff\x3d\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4f\x00\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x39\x00\x02\x00\x03\x00\x04\x00\x3d\x00\xff\xff\xff\xff\x3e\x00\x3f\x00\x40\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x4f\x00\xff\xff\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x04\x00\xff\xff\x06\x00\xff\xff\x22\x00\x23\x00\xff\xff\x0b\x00\x26\x00\x27\x00\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\xff\xff\xff\xff\x1c\x00\x1d\x00\xff\xff\xff\xff\xff\xff\x04\x00\x22\x00\x23\x00\xff\xff\x08\x00\x26\x00\x27\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\x1d\x00\x04\x00\xff\xff\x06\x00\xff\xff\x22\x00\x23\x00\xff\xff\x0b\x00\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\xff\xff\xff\xff\x1c\x00\x1d\x00\xff\xff\xff\xff\xff\xff\x04\x00\x22\x00\x23\x00\xff\xff\xff\xff\x26\x00\x27\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\xff\xff\x18\x00\xff\xff\xff\xff\xff\xff\x1c\x00\x1d\x00\x04\x00\xff\xff\x06\x00\xff\xff\x22\x00\x23\x00\xff\xff\x0b\x00\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\xff\xff\xff\xff\x1c\x00\x1d\x00\xff\xff\xff\xff\xff\xff\x04\x00\x22\x00\x23\x00\xff\xff\xff\xff\x26\x00\x27\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\xff\xff\x18\x00\xff\xff\xff\xff\xff\xff\x1c\x00\x1d\x00\x04\x00\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\xff\xff\x0b\x00\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x04\x00\xff\xff\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x0b\x00\xff\xff\x1c\x00\x1d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\xff\xff\xff\xff\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\x1c\x00\x1d\x00\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x15\x00\x67\x00\x27\x00\x17\x00\x03\x00\xa6\x00\x1b\x00\x66\x00\x66\x00\x68\x00\x9f\x00\xa0\x00\x69\x00\x6a\x00\x27\x00\x4a\x00\x90\x00\x91\x00\xff\xff\x6b\x00\x6c\x00\x29\x00\x13\x00\x06\x00\x07\x00\x6d\x00\x2a\x00\x2b\x00\x25\x00\x1c\x00\x13\x00\x13\x00\x2c\x00\x2d\x00\xab\x00\xcd\x00\x2e\x00\x2f\x00\x2a\x00\x2b\x00\xa6\x00\xa6\x00\xa6\x00\x0d\x00\x0e\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x1b\x00\x15\x00\x1b\x00\x15\x00\x6e\x00\x27\x00\x24\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x29\x00\x15\x00\x15\x00\xa4\x00\x0a\x00\x6f\x00\x15\x00\x28\x00\x18\x00\x22\x00\x60\x00\x1c\x00\x70\x00\x1c\x00\x71\x00\x72\x00\xe0\x00\x2a\x00\x2b\x00\xa7\x00\x73\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x0e\x00\x1b\x00\x1a\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\xc4\x00\x41\x00\x27\x00\x43\x00\x28\x00\xd9\x00\xa4\x00\x20\x00\xa4\x00\x29\x00\x94\x00\x49\x00\x61\x00\xd5\x00\xd3\x00\xe7\x00\x1c\x00\x62\x00\x63\x00\x03\x00\x1b\x00\x89\x00\x16\x00\x1f\x00\xe8\x00\x13\x00\x2a\x00\x2b\x00\x64\x00\x1a\x00\x21\x00\x27\x00\x2c\x00\x2d\x00\xd4\x00\xd8\x00\x2e\x00\x2f\x00\x29\x00\xeb\x00\x1b\x00\xe1\x00\x41\x00\x1c\x00\x43\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x0e\x00\x60\x00\xa4\x00\xa4\x00\x7e\x00\x2a\x00\x2b\x00\x20\x00\xa8\x00\x36\x00\xa9\x00\x2c\x00\x2d\x00\x1c\x00\xa2\x00\x2e\x00\x2f\x00\x1a\x00\x9f\x00\xa0\x00\x25\x00\x8c\x00\x20\x00\x1b\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\xa4\x00\x21\x00\x1b\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x21\x00\x1c\x00\x94\x00\x49\x00\x81\x00\x45\x00\xb2\x00\xb3\x00\x7e\x00\x1c\x00\x1f\x00\x13\x00\x06\x00\x07\x00\x7c\x00\xb4\x00\xb5\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x27\x00\x1f\x00\x28\x00\x0e\x00\x13\x00\x14\x00\x1a\x00\x29\x00\xa4\x00\x03\x00\x04\x00\x05\x00\x03\x00\x08\x00\x09\x00\x20\x00\x1d\x00\x24\x00\x60\x00\x8c\x00\x8d\x00\x8e\x00\x1a\x00\x7a\x00\x2a\x00\x2b\x00\x0a\x00\x0b\x00\x24\x00\x27\x00\x2c\x00\x2d\x00\x20\x00\x1f\x00\x2e\x00\x2f\x00\x29\x00\xc9\x00\x21\x00\x9c\x00\x9d\x00\x0e\x00\xae\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\xc7\x00\x60\x00\x91\x00\x92\x00\xb1\x00\x2a\x00\x2b\x00\x21\x00\xa8\x00\x36\x00\xe2\x00\x2c\x00\x2d\x00\x61\x00\xaf\x00\x2e\x00\x2f\x00\xae\x00\xcb\x00\x63\x00\x9c\x00\x9d\x00\x91\x00\x92\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x64\x00\x7e\x00\xdf\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x99\x00\x9a\x00\xa4\x00\xc7\x00\xa2\x00\x03\x00\xe5\x00\xb3\x00\xb1\x00\x77\x00\x1a\x00\xe5\x00\x03\x00\x9b\x00\x9c\x00\xb4\x00\xb5\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x27\x00\x22\x00\x28\x00\x45\x00\x66\x00\xa0\x00\x5f\x00\x29\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x0e\x00\xa5\x00\xa2\x00\x8a\x00\x7c\x00\x7e\x00\x2a\x00\x2b\x00\xd2\x00\x56\x00\x0f\x00\x10\x00\x2c\x00\x2d\x00\x4a\x00\xd1\x00\x2e\x00\x2f\x00\x94\x00\x95\x00\x11\x00\xce\x00\xc5\x00\x2a\x00\x2b\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x0e\x00\x96\x00\x97\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x35\x00\x36\x00\x26\x00\x10\x00\x83\x00\x31\x00\x32\x00\x33\x00\x34\x00\xcc\x00\xc7\x00\xaf\x00\x11\x00\x27\x00\x4a\x00\x28\x00\xa2\x00\x80\x00\xac\x00\xdf\x00\x29\x00\x84\x00\x85\x00\x86\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x2a\x00\x2b\x00\xa0\x00\xdc\x00\xd7\x00\x27\x00\x2c\x00\x2d\x00\xd6\x00\xcf\x00\x2e\x00\x2f\x00\x29\x00\xe3\x00\x00\x00\xe9\x00\x87\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x47\x00\x48\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x00\x00\xc9\x00\x36\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x13\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x2a\x00\x2b\x00\x27\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x29\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaa\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\xb1\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x2a\x00\x2b\x00\x27\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x29\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x2a\x00\x2b\x00\x27\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x29\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb7\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb8\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x2a\x00\x2b\x00\x27\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x29\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x50\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x00\x00\xba\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x57\x00\x00\x00\x00\x00\x00\x00\x58\x00\xb9\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x2a\x00\x2b\x00\x27\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x59\x00\x29\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x50\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\xbe\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x82\x00\x00\x00\x00\x00\x00\x00\x58\x00\x00\x00\xbd\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x2a\x00\x2b\x00\x27\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x59\x00\x29\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x50\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\xbc\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x58\x00\x00\x00\xbb\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x2a\x00\x2b\x00\x27\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x59\x00\x29\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\xc0\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x27\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x29\x00\x2e\x00\x2f\x00\xbf\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x2a\x00\x2b\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x88\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x83\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x27\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x00\x00\x29\x00\x2e\x00\x2f\x00\xe6\x00\xc3\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x27\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x2d\x00\x27\x00\x29\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x29\x00\x87\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x03\x00\x00\x00\x1a\x00\xc2\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x46\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x50\x00\x47\x00\x48\x00\x00\x00\xc1\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x00\x00\xe1\x00\x41\x00\x50\x00\x43\x00\x00\x00\x00\x00\xea\x00\x41\x00\x00\x00\x43\x00\x00\x00\x00\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\x00\x00\x00\x00\x00\x00\x00\x58\x00\x2a\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x59\x00\x00\x00\x83\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xda\x00\x03\x00\x75\x00\x1a\x00\x58\x00\x00\x00\x00\x00\x84\x00\xdb\x00\x86\x00\x46\x00\x00\x00\x00\x00\x76\x00\x77\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x78\x00\x79\x00\x59\x00\x00\x00\x0d\x00\x0e\x00\x7a\x00\x47\x00\x48\x00\x1a\x00\x00\x00\x45\x00\x00\x00\x49\x00\x4a\x00\x00\x00\x46\x00\x4b\x00\x4c\x00\x87\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x00\x00\x00\x00\x47\x00\x48\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x49\x00\x4a\x00\x00\x00\x24\x00\x4b\x00\x4c\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x1a\x00\x00\x00\x45\x00\x00\x00\x49\x00\x4a\x00\x00\x00\x46\x00\x4b\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x00\x00\x00\x00\x47\x00\x48\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x4b\x00\x4c\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x00\x00\x66\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x1a\x00\x00\x00\x45\x00\x00\x00\x49\x00\x4a\x00\x00\x00\x46\x00\x4b\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x00\x00\x00\x00\x47\x00\x48\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x4b\x00\x4c\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x00\x00\x66\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x49\x00\x4a\x00\x00\x00\x46\x00\x4b\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x13\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x46\x00\x00\x00\x47\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x4b\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x00\x00\x00\x00\x13\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 161) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149),
	(150 , happyReduce_150),
	(151 , happyReduce_151),
	(152 , happyReduce_152),
	(153 , happyReduce_153),
	(154 , happyReduce_154),
	(155 , happyReduce_155),
	(156 , happyReduce_156),
	(157 , happyReduce_157),
	(158 , happyReduce_158),
	(159 , happyReduce_159),
	(160 , happyReduce_160),
	(161 , happyReduce_161)
	]

happy_n_terms = 53 :: Int
happy_n_nonterms = 95 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn4
		 (POpenGraph (mkPosToken happy_var_1)
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (PCloseGraph (mkPosToken happy_var_1)
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (POpenParenthesis (mkPosToken happy_var_1)
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (PCloseParenthesis (mkPosToken happy_var_1)
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (POpenBracket (mkPosToken happy_var_1)
	)}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (PCloseBracket (mkPosToken happy_var_1)
	)}

happyReduce_7 = happySpecReduce_1  6# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (PSemicolon (mkPosToken happy_var_1)
	)}

happyReduce_8 = happySpecReduce_1  7# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (PColon (mkPosToken happy_var_1)
	)}

happyReduce_9 = happySpecReduce_1  8# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (PPoint (mkPosToken happy_var_1)
	)}

happyReduce_10 = happySpecReduce_1  9# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (PIf (mkPosToken happy_var_1)
	)}

happyReduce_11 = happySpecReduce_1  10# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (PThen (mkPosToken happy_var_1)
	)}

happyReduce_12 = happySpecReduce_1  11# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (PElse (mkPosToken happy_var_1)
	)}

happyReduce_13 = happySpecReduce_1  12# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (Pdo (mkPosToken happy_var_1)
	)}

happyReduce_14 = happySpecReduce_1  13# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (PWhile (mkPosToken happy_var_1)
	)}

happyReduce_15 = happySpecReduce_1  14# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (PIntType (mkPosToken happy_var_1)
	)}

happyReduce_16 = happySpecReduce_1  15# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (PRealType (mkPosToken happy_var_1)
	)}

happyReduce_17 = happySpecReduce_1  16# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (PCharType (mkPosToken happy_var_1)
	)}

happyReduce_18 = happySpecReduce_1  17# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (PBoolType (mkPosToken happy_var_1)
	)}

happyReduce_19 = happySpecReduce_1  18# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (PStringType (mkPosToken happy_var_1)
	)}

happyReduce_20 = happySpecReduce_1  19# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (PBreak (mkPosToken happy_var_1)
	)}

happyReduce_21 = happySpecReduce_1  20# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (PContinue (mkPosToken happy_var_1)
	)}

happyReduce_22 = happySpecReduce_1  21# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (PAssignmEq (mkPosToken happy_var_1)
	)}

happyReduce_23 = happySpecReduce_1  22# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (PRef (mkPosToken happy_var_1)
	)}

happyReduce_24 = happySpecReduce_1  23# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (PVar (mkPosToken happy_var_1)
	)}

happyReduce_25 = happySpecReduce_1  24# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (PProc (mkPosToken happy_var_1)
	)}

happyReduce_26 = happySpecReduce_1  25# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (PReturn (mkPosToken happy_var_1)
	)}

happyReduce_27 = happySpecReduce_1  26# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (PTrue (mkPosToken happy_var_1)
	)}

happyReduce_28 = happySpecReduce_1  27# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (PFalse (mkPosToken happy_var_1)
	)}

happyReduce_29 = happySpecReduce_1  28# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (PEpow (mkPosToken happy_var_1)
	)}

happyReduce_30 = happySpecReduce_1  29# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (PElthen (mkPosToken happy_var_1)
	)}

happyReduce_31 = happySpecReduce_1  30# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (PEgrthen (mkPosToken happy_var_1)
	)}

happyReduce_32 = happySpecReduce_1  31# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (PEplus (mkPosToken happy_var_1)
	)}

happyReduce_33 = happySpecReduce_1  32# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (PEminus (mkPosToken happy_var_1)
	)}

happyReduce_34 = happySpecReduce_1  33# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (PEtimes (mkPosToken happy_var_1)
	)}

happyReduce_35 = happySpecReduce_1  34# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (PEdiv (mkPosToken happy_var_1)
	)}

happyReduce_36 = happySpecReduce_1  35# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (PEmod (mkPosToken happy_var_1)
	)}

happyReduce_37 = happySpecReduce_1  36# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (PDef (mkPosToken happy_var_1)
	)}

happyReduce_38 = happySpecReduce_1  37# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (PNeg (mkPosToken happy_var_1)
	)}

happyReduce_39 = happySpecReduce_1  38# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (PElor (mkPosToken happy_var_1)
	)}

happyReduce_40 = happySpecReduce_1  39# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (PEland (mkPosToken happy_var_1)
	)}

happyReduce_41 = happySpecReduce_1  40# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (PEeq (mkPosToken happy_var_1)
	)}

happyReduce_42 = happySpecReduce_1  41# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (PEneq (mkPosToken happy_var_1)
	)}

happyReduce_43 = happySpecReduce_1  42# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (PEle (mkPosToken happy_var_1)
	)}

happyReduce_44 = happySpecReduce_1  43# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (PEge (mkPosToken happy_var_1)
	)}

happyReduce_45 = happySpecReduce_1  44# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (PAssignmPlus (mkPosToken happy_var_1)
	)}

happyReduce_46 = happySpecReduce_1  45# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (PIdent (mkPosToken happy_var_1)
	)}

happyReduce_47 = happySpecReduce_1  46# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (PString (mkPosToken happy_var_1)
	)}

happyReduce_48 = happySpecReduce_1  47# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (PChar (mkPosToken happy_var_1)
	)}

happyReduce_49 = happySpecReduce_1  48# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (PDouble (mkPosToken happy_var_1)
	)}

happyReduce_50 = happySpecReduce_1  49# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn53
		 (PInteger (mkPosToken happy_var_1)
	)}

happyReduce_51 = happySpecReduce_1  50# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	happyIn54
		 (AbsChapel.Progr happy_var_1
	)}

happyReduce_52 = happySpecReduce_1  51# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	happyIn55
		 (AbsChapel.Mod (reverse happy_var_1)
	)}

happyReduce_53 = happySpecReduce_0  52# happyReduction_53
happyReduction_53  =  happyIn56
		 ([]
	)

happyReduce_54 = happySpecReduce_2  52# happyReduction_54
happyReduction_54 happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn56
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_55 = happySpecReduce_1  53# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	happyIn57
		 (AbsChapel.ExtDecl happy_var_1
	)}

happyReduce_56 = happySpecReduce_1  53# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	happyIn57
		 (AbsChapel.ExtFun happy_var_1
	)}

happyReduce_57 = happySpecReduce_3  54# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut70 happy_x_1 of { (HappyWrap70 happy_var_1) -> 
	case happyOut59 happy_x_2 of { (HappyWrap59 happy_var_2) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn58
		 (AbsChapel.Decl happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_58 = happySpecReduce_1  55# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut60 happy_x_1 of { (HappyWrap60 happy_var_1) -> 
	happyIn59
		 ((:[]) happy_var_1
	)}

happyReduce_59 = happySpecReduce_3  55# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { (HappyWrap60 happy_var_1) -> 
	case happyOut59 happy_x_3 of { (HappyWrap59 happy_var_3) -> 
	happyIn59
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_60 = happySpecReduce_3  56# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut69 happy_x_1 of { (HappyWrap69 happy_var_1) -> 
	case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	case happyOut61 happy_x_3 of { (HappyWrap61 happy_var_3) -> 
	happyIn60
		 (AbsChapel.NoAssgmArrayDec happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_61 = happyReduce 5# 56# happyReduction_61
happyReduction_61 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut69 happy_x_1 of { (HappyWrap69 happy_var_1) -> 
	case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	case happyOut61 happy_x_3 of { (HappyWrap61 happy_var_3) -> 
	case happyOut84 happy_x_4 of { (HappyWrap84 happy_var_4) -> 
	case happyOut62 happy_x_5 of { (HappyWrap62 happy_var_5) -> 
	happyIn60
		 (AbsChapel.AssgmTypeDec happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}}}

happyReduce_62 = happySpecReduce_3  56# happyReduction_62
happyReduction_62 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut69 happy_x_1 of { (HappyWrap69 happy_var_1) -> 
	case happyOut84 happy_x_2 of { (HappyWrap84 happy_var_2) -> 
	case happyOut62 happy_x_3 of { (HappyWrap62 happy_var_3) -> 
	happyIn60
		 (AbsChapel.AssgmDec happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_63 = happySpecReduce_1  57# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut83 happy_x_1 of { (HappyWrap83 happy_var_1) -> 
	happyIn61
		 (AbsChapel.TypeSpecNorm happy_var_1
	)}

happyReduce_64 = happySpecReduce_2  57# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	case happyOut83 happy_x_2 of { (HappyWrap83 happy_var_2) -> 
	happyIn61
		 (AbsChapel.TypeSpecAr happy_var_1 happy_var_2
	)}}

happyReduce_65 = happySpecReduce_1  58# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut63 happy_x_1 of { (HappyWrap63 happy_var_1) -> 
	happyIn62
		 (AbsChapel.ExprDecArray happy_var_1
	)}

happyReduce_66 = happySpecReduce_1  58# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut86 happy_x_1 of { (HappyWrap86 happy_var_1) -> 
	happyIn62
		 (AbsChapel.ExprDec happy_var_1
	)}

happyReduce_67 = happySpecReduce_3  59# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut64 happy_x_2 of { (HappyWrap64 happy_var_2) -> 
	case happyOut9 happy_x_3 of { (HappyWrap9 happy_var_3) -> 
	happyIn63
		 (AbsChapel.ArrayInit happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_68 = happySpecReduce_1  60# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	happyIn64
		 ((:[]) happy_var_1
	)}

happyReduce_69 = happySpecReduce_3  60# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	case happyOut64 happy_x_3 of { (HappyWrap64 happy_var_3) -> 
	happyIn64
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_70 = happySpecReduce_3  61# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut67 happy_x_2 of { (HappyWrap67 happy_var_2) -> 
	case happyOut9 happy_x_3 of { (HappyWrap9 happy_var_3) -> 
	happyIn65
		 (AbsChapel.ArrayDeclIndex happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_71 = happyReduce 4# 62# happyReduction_71
happyReduction_71 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut68 happy_x_1 of { (HappyWrap68 happy_var_1) -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	case happyOut68 happy_x_4 of { (HappyWrap68 happy_var_4) -> 
	happyIn66
		 (AbsChapel.ArrayDimSingle happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_72 = happySpecReduce_1  62# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut68 happy_x_1 of { (HappyWrap68 happy_var_1) -> 
	happyIn66
		 (AbsChapel.ArrayDimBound happy_var_1
	)}

happyReduce_73 = happySpecReduce_1  63# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut66 happy_x_1 of { (HappyWrap66 happy_var_1) -> 
	happyIn67
		 ((:[]) happy_var_1
	)}

happyReduce_74 = happySpecReduce_3  63# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut66 happy_x_1 of { (HappyWrap66 happy_var_1) -> 
	case happyOut67 happy_x_3 of { (HappyWrap67 happy_var_3) -> 
	happyIn67
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_75 = happySpecReduce_1  64# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn68
		 (AbsChapel.ArrayBoundIdent happy_var_1
	)}

happyReduce_76 = happySpecReduce_1  64# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut98 happy_x_1 of { (HappyWrap98 happy_var_1) -> 
	happyIn68
		 (AbsChapel.ArratBoundConst happy_var_1
	)}

happyReduce_77 = happySpecReduce_1  65# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn69
		 ((:[]) happy_var_1
	)}

happyReduce_78 = happySpecReduce_3  65# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut69 happy_x_3 of { (HappyWrap69 happy_var_3) -> 
	happyIn69
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_79 = happySpecReduce_1  66# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	happyIn70
		 (AbsChapel.PVarMode happy_var_1
	)}

happyReduce_80 = happySpecReduce_3  67# happyReduction_80
happyReduction_80 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut72 happy_x_2 of { (HappyWrap72 happy_var_2) -> 
	case happyOut78 happy_x_3 of { (HappyWrap78 happy_var_3) -> 
	happyIn71
		 (AbsChapel.FunDec happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_81 = happySpecReduce_2  68# happyReduction_81
happyReduction_81 happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut73 happy_x_2 of { (HappyWrap73 happy_var_2) -> 
	happyIn72
		 (AbsChapel.SignNoRet happy_var_1 happy_var_2
	)}}

happyReduce_82 = happyReduce 4# 68# happyReduction_82
happyReduction_82 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut73 happy_x_2 of { (HappyWrap73 happy_var_2) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	case happyOut61 happy_x_4 of { (HappyWrap61 happy_var_4) -> 
	happyIn72
		 (AbsChapel.SignWRet happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_83 = happySpecReduce_3  69# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut74 happy_x_2 of { (HappyWrap74 happy_var_2) -> 
	case happyOut7 happy_x_3 of { (HappyWrap7 happy_var_3) -> 
	happyIn73
		 (AbsChapel.FunParams happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_84 = happySpecReduce_0  70# happyReduction_84
happyReduction_84  =  happyIn74
		 ([]
	)

happyReduce_85 = happySpecReduce_1  70# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut75 happy_x_1 of { (HappyWrap75 happy_var_1) -> 
	happyIn74
		 ((:[]) happy_var_1
	)}

happyReduce_86 = happySpecReduce_3  70# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut75 happy_x_1 of { (HappyWrap75 happy_var_1) -> 
	case happyOut74 happy_x_3 of { (HappyWrap74 happy_var_3) -> 
	happyIn74
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_87 = happySpecReduce_3  71# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut69 happy_x_1 of { (HappyWrap69 happy_var_1) -> 
	case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	case happyOut61 happy_x_3 of { (HappyWrap61 happy_var_3) -> 
	happyIn75
		 (AbsChapel.ParNoMode happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_88 = happyReduce 4# 71# happyReduction_88
happyReduction_88 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut85 happy_x_1 of { (HappyWrap85 happy_var_1) -> 
	case happyOut69 happy_x_2 of { (HappyWrap69 happy_var_2) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	case happyOut61 happy_x_4 of { (HappyWrap61 happy_var_4) -> 
	happyIn75
		 (AbsChapel.ParWMode happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_89 = happySpecReduce_0  72# happyReduction_89
happyReduction_89  =  happyIn76
		 ([]
	)

happyReduce_90 = happySpecReduce_1  72# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut77 happy_x_1 of { (HappyWrap77 happy_var_1) -> 
	happyIn76
		 ((:[]) happy_var_1
	)}

happyReduce_91 = happySpecReduce_3  72# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut77 happy_x_1 of { (HappyWrap77 happy_var_1) -> 
	case happyOut76 happy_x_3 of { (HappyWrap76 happy_var_3) -> 
	happyIn76
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_92 = happySpecReduce_1  73# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut86 happy_x_1 of { (HappyWrap86 happy_var_1) -> 
	happyIn77
		 (AbsChapel.PassedPar happy_var_1
	)}

happyReduce_93 = happySpecReduce_2  73# happyReduction_93
happyReduction_93 happy_x_2
	happy_x_1
	 =  case happyOut85 happy_x_1 of { (HappyWrap85 happy_var_1) -> 
	case happyOut86 happy_x_2 of { (HappyWrap86 happy_var_2) -> 
	happyIn77
		 (AbsChapel.PassedParWMode happy_var_1 happy_var_2
	)}}

happyReduce_94 = happySpecReduce_3  74# happyReduction_94
happyReduction_94 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut79 happy_x_2 of { (HappyWrap79 happy_var_2) -> 
	case happyOut5 happy_x_3 of { (HappyWrap5 happy_var_3) -> 
	happyIn78
		 (AbsChapel.BodyBlock happy_var_1 (reverse happy_var_2) happy_var_3
	)}}}

happyReduce_95 = happySpecReduce_0  75# happyReduction_95
happyReduction_95  =  happyIn79
		 ([]
	)

happyReduce_96 = happySpecReduce_2  75# happyReduction_96
happyReduction_96 happy_x_2
	happy_x_1
	 =  case happyOut79 happy_x_1 of { (HappyWrap79 happy_var_1) -> 
	case happyOut80 happy_x_2 of { (HappyWrap80 happy_var_2) -> 
	happyIn79
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_97 = happySpecReduce_1  76# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOut81 happy_x_1 of { (HappyWrap81 happy_var_1) -> 
	happyIn80
		 (AbsChapel.Stm happy_var_1
	)}

happyReduce_98 = happySpecReduce_2  76# happyReduction_98
happyReduction_98 happy_x_2
	happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	happyIn80
		 (AbsChapel.Fun happy_var_1 happy_var_2
	)}}

happyReduce_99 = happySpecReduce_1  76# happyReduction_99
happyReduction_99 happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	happyIn80
		 (AbsChapel.DeclStm happy_var_1
	)}

happyReduce_100 = happySpecReduce_1  76# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut78 happy_x_1 of { (HappyWrap78 happy_var_1) -> 
	happyIn80
		 (AbsChapel.Block happy_var_1
	)}

happyReduce_101 = happyReduce 4# 77# happyReduction_101
happyReduction_101 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut78 happy_x_2 of { (HappyWrap78 happy_var_2) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	case happyOut82 happy_x_4 of { (HappyWrap82 happy_var_4) -> 
	happyIn81
		 (AbsChapel.DoWhile happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_102 = happySpecReduce_3  77# happyReduction_102
happyReduction_102 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	case happyOut82 happy_x_2 of { (HappyWrap82 happy_var_2) -> 
	case happyOut78 happy_x_3 of { (HappyWrap78 happy_var_3) -> 
	happyIn81
		 (AbsChapel.While happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_103 = happyReduce 4# 77# happyReduction_103
happyReduction_103 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	case happyOut82 happy_x_2 of { (HappyWrap82 happy_var_2) -> 
	case happyOut14 happy_x_3 of { (HappyWrap14 happy_var_3) -> 
	case happyOut78 happy_x_4 of { (HappyWrap78 happy_var_4) -> 
	happyIn81
		 (AbsChapel.If happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_104 = happyReduce 6# 77# happyReduction_104
happyReduction_104 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	case happyOut82 happy_x_2 of { (HappyWrap82 happy_var_2) -> 
	case happyOut14 happy_x_3 of { (HappyWrap14 happy_var_3) -> 
	case happyOut78 happy_x_4 of { (HappyWrap78 happy_var_4) -> 
	case happyOut15 happy_x_5 of { (HappyWrap15 happy_var_5) -> 
	case happyOut78 happy_x_6 of { (HappyWrap78 happy_var_6) -> 
	happyIn81
		 (AbsChapel.IfElse happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}}

happyReduce_105 = happySpecReduce_3  77# happyReduction_105
happyReduction_105 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut86 happy_x_2 of { (HappyWrap86 happy_var_2) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn81
		 (AbsChapel.RetVal happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_106 = happySpecReduce_2  77# happyReduction_106
happyReduction_106 happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	happyIn81
		 (AbsChapel.RetVoid happy_var_1 happy_var_2
	)}}

happyReduce_107 = happySpecReduce_2  77# happyReduction_107
happyReduction_107 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	happyIn81
		 (AbsChapel.Continue happy_var_1 happy_var_2
	)}}

happyReduce_108 = happySpecReduce_2  77# happyReduction_108
happyReduction_108 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	happyIn81
		 (AbsChapel.Break happy_var_1 happy_var_2
	)}}

happyReduce_109 = happySpecReduce_2  77# happyReduction_109
happyReduction_109 happy_x_2
	happy_x_1
	 =  case happyOut86 happy_x_1 of { (HappyWrap86 happy_var_1) -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	happyIn81
		 (AbsChapel.StExp happy_var_1 happy_var_2
	)}}

happyReduce_110 = happySpecReduce_3  78# happyReduction_110
happyReduction_110 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut86 happy_x_2 of { (HappyWrap86 happy_var_2) -> 
	case happyOut7 happy_x_3 of { (HappyWrap7 happy_var_3) -> 
	happyIn82
		 (AbsChapel.SGuard happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_111 = happySpecReduce_1  79# happyReduction_111
happyReduction_111 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn83
		 (AbsChapel.Tint happy_var_1
	)}

happyReduce_112 = happySpecReduce_1  79# happyReduction_112
happyReduction_112 happy_x_1
	 =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
	happyIn83
		 (AbsChapel.Treal happy_var_1
	)}

happyReduce_113 = happySpecReduce_1  79# happyReduction_113
happyReduction_113 happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn83
		 (AbsChapel.Tchar happy_var_1
	)}

happyReduce_114 = happySpecReduce_1  79# happyReduction_114
happyReduction_114 happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	happyIn83
		 (AbsChapel.Tstring happy_var_1
	)}

happyReduce_115 = happySpecReduce_1  79# happyReduction_115
happyReduction_115 happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	happyIn83
		 (AbsChapel.Tbool happy_var_1
	)}

happyReduce_116 = happySpecReduce_2  79# happyReduction_116
happyReduction_116 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	case happyOut61 happy_x_2 of { (HappyWrap61 happy_var_2) -> 
	happyIn83
		 (AbsChapel.TPointer happy_var_1 happy_var_2
	)}}

happyReduce_117 = happySpecReduce_1  80# happyReduction_117
happyReduction_117 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn84
		 (AbsChapel.AssgnEq happy_var_1
	)}

happyReduce_118 = happySpecReduce_1  80# happyReduction_118
happyReduction_118 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn84
		 (AbsChapel.AssgnPlEq happy_var_1
	)}

happyReduce_119 = happySpecReduce_1  81# happyReduction_119
happyReduction_119 happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn85
		 (AbsChapel.RefMode happy_var_1
	)}

happyReduce_120 = happySpecReduce_3  82# happyReduction_120
happyReduction_120 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut86 happy_x_1 of { (HappyWrap86 happy_var_1) -> 
	case happyOut84 happy_x_2 of { (HappyWrap84 happy_var_2) -> 
	case happyOut87 happy_x_3 of { (HappyWrap87 happy_var_3) -> 
	happyIn86
		 (AbsChapel.EAss happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_121 = happySpecReduce_1  82# happyReduction_121
happyReduction_121 happy_x_1
	 =  case happyOut87 happy_x_1 of { (HappyWrap87 happy_var_1) -> 
	happyIn86
		 (happy_var_1
	)}

happyReduce_122 = happySpecReduce_3  83# happyReduction_122
happyReduction_122 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut87 happy_x_1 of { (HappyWrap87 happy_var_1) -> 
	case happyOut42 happy_x_2 of { (HappyWrap42 happy_var_2) -> 
	case happyOut88 happy_x_3 of { (HappyWrap88 happy_var_3) -> 
	happyIn87
		 (AbsChapel.Elor happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_123 = happySpecReduce_1  83# happyReduction_123
happyReduction_123 happy_x_1
	 =  case happyOut88 happy_x_1 of { (HappyWrap88 happy_var_1) -> 
	happyIn87
		 (happy_var_1
	)}

happyReduce_124 = happySpecReduce_3  84# happyReduction_124
happyReduction_124 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut88 happy_x_1 of { (HappyWrap88 happy_var_1) -> 
	case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	case happyOut89 happy_x_3 of { (HappyWrap89 happy_var_3) -> 
	happyIn88
		 (AbsChapel.Eland happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_125 = happySpecReduce_1  84# happyReduction_125
happyReduction_125 happy_x_1
	 =  case happyOut89 happy_x_1 of { (HappyWrap89 happy_var_1) -> 
	happyIn88
		 (happy_var_1
	)}

happyReduce_126 = happySpecReduce_3  85# happyReduction_126
happyReduction_126 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut89 happy_x_1 of { (HappyWrap89 happy_var_1) -> 
	case happyOut44 happy_x_2 of { (HappyWrap44 happy_var_2) -> 
	case happyOut90 happy_x_3 of { (HappyWrap90 happy_var_3) -> 
	happyIn89
		 (AbsChapel.Eeq happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_127 = happySpecReduce_3  85# happyReduction_127
happyReduction_127 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut89 happy_x_1 of { (HappyWrap89 happy_var_1) -> 
	case happyOut45 happy_x_2 of { (HappyWrap45 happy_var_2) -> 
	case happyOut90 happy_x_3 of { (HappyWrap90 happy_var_3) -> 
	happyIn89
		 (AbsChapel.Eneq happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_128 = happySpecReduce_1  85# happyReduction_128
happyReduction_128 happy_x_1
	 =  case happyOut90 happy_x_1 of { (HappyWrap90 happy_var_1) -> 
	happyIn89
		 (happy_var_1
	)}

happyReduce_129 = happySpecReduce_3  86# happyReduction_129
happyReduction_129 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut90 happy_x_1 of { (HappyWrap90 happy_var_1) -> 
	case happyOut33 happy_x_2 of { (HappyWrap33 happy_var_2) -> 
	case happyOut91 happy_x_3 of { (HappyWrap91 happy_var_3) -> 
	happyIn90
		 (AbsChapel.Elthen happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_130 = happySpecReduce_3  86# happyReduction_130
happyReduction_130 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut90 happy_x_1 of { (HappyWrap90 happy_var_1) -> 
	case happyOut34 happy_x_2 of { (HappyWrap34 happy_var_2) -> 
	case happyOut91 happy_x_3 of { (HappyWrap91 happy_var_3) -> 
	happyIn90
		 (AbsChapel.Egrthen happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_131 = happySpecReduce_3  86# happyReduction_131
happyReduction_131 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut90 happy_x_1 of { (HappyWrap90 happy_var_1) -> 
	case happyOut46 happy_x_2 of { (HappyWrap46 happy_var_2) -> 
	case happyOut91 happy_x_3 of { (HappyWrap91 happy_var_3) -> 
	happyIn90
		 (AbsChapel.Ele happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_132 = happySpecReduce_3  86# happyReduction_132
happyReduction_132 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut90 happy_x_1 of { (HappyWrap90 happy_var_1) -> 
	case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	case happyOut91 happy_x_3 of { (HappyWrap91 happy_var_3) -> 
	happyIn90
		 (AbsChapel.Ege happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_133 = happySpecReduce_1  86# happyReduction_133
happyReduction_133 happy_x_1
	 =  case happyOut91 happy_x_1 of { (HappyWrap91 happy_var_1) -> 
	happyIn90
		 (happy_var_1
	)}

happyReduce_134 = happySpecReduce_3  87# happyReduction_134
happyReduction_134 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut91 happy_x_1 of { (HappyWrap91 happy_var_1) -> 
	case happyOut35 happy_x_2 of { (HappyWrap35 happy_var_2) -> 
	case happyOut92 happy_x_3 of { (HappyWrap92 happy_var_3) -> 
	happyIn91
		 (AbsChapel.Eplus happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_135 = happySpecReduce_3  87# happyReduction_135
happyReduction_135 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut91 happy_x_1 of { (HappyWrap91 happy_var_1) -> 
	case happyOut36 happy_x_2 of { (HappyWrap36 happy_var_2) -> 
	case happyOut92 happy_x_3 of { (HappyWrap92 happy_var_3) -> 
	happyIn91
		 (AbsChapel.Eminus happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_136 = happySpecReduce_1  87# happyReduction_136
happyReduction_136 happy_x_1
	 =  case happyOut92 happy_x_1 of { (HappyWrap92 happy_var_1) -> 
	happyIn91
		 (happy_var_1
	)}

happyReduce_137 = happySpecReduce_3  88# happyReduction_137
happyReduction_137 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut92 happy_x_1 of { (HappyWrap92 happy_var_1) -> 
	case happyOut37 happy_x_2 of { (HappyWrap37 happy_var_2) -> 
	case happyOut93 happy_x_3 of { (HappyWrap93 happy_var_3) -> 
	happyIn92
		 (AbsChapel.Etimes happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_138 = happySpecReduce_3  88# happyReduction_138
happyReduction_138 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut92 happy_x_1 of { (HappyWrap92 happy_var_1) -> 
	case happyOut38 happy_x_2 of { (HappyWrap38 happy_var_2) -> 
	case happyOut93 happy_x_3 of { (HappyWrap93 happy_var_3) -> 
	happyIn92
		 (AbsChapel.Ediv happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_139 = happySpecReduce_3  88# happyReduction_139
happyReduction_139 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut92 happy_x_1 of { (HappyWrap92 happy_var_1) -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	case happyOut93 happy_x_3 of { (HappyWrap93 happy_var_3) -> 
	happyIn92
		 (AbsChapel.Emod happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_140 = happySpecReduce_1  88# happyReduction_140
happyReduction_140 happy_x_1
	 =  case happyOut93 happy_x_1 of { (HappyWrap93 happy_var_1) -> 
	happyIn92
		 (happy_var_1
	)}

happyReduce_141 = happySpecReduce_2  89# happyReduction_141
happyReduction_141 happy_x_2
	happy_x_1
	 =  case happyOut97 happy_x_1 of { (HappyWrap97 happy_var_1) -> 
	case happyOut93 happy_x_2 of { (HappyWrap93 happy_var_2) -> 
	happyIn93
		 (AbsChapel.Epreop happy_var_1 happy_var_2
	)}}

happyReduce_142 = happySpecReduce_1  89# happyReduction_142
happyReduction_142 happy_x_1
	 =  case happyOut94 happy_x_1 of { (HappyWrap94 happy_var_1) -> 
	happyIn93
		 (happy_var_1
	)}

happyReduce_143 = happySpecReduce_3  90# happyReduction_143
happyReduction_143 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut94 happy_x_1 of { (HappyWrap94 happy_var_1) -> 
	case happyOut32 happy_x_2 of { (HappyWrap32 happy_var_2) -> 
	case happyOut95 happy_x_3 of { (HappyWrap95 happy_var_3) -> 
	happyIn94
		 (AbsChapel.Epow happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_144 = happySpecReduce_2  90# happyReduction_144
happyReduction_144 happy_x_2
	happy_x_1
	 =  case happyOut95 happy_x_1 of { (HappyWrap95 happy_var_1) -> 
	case happyOut63 happy_x_2 of { (HappyWrap63 happy_var_2) -> 
	happyIn94
		 (AbsChapel.Earray happy_var_1 happy_var_2
	)}}

happyReduce_145 = happySpecReduce_1  90# happyReduction_145
happyReduction_145 happy_x_1
	 =  case happyOut95 happy_x_1 of { (HappyWrap95 happy_var_1) -> 
	happyIn94
		 (happy_var_1
	)}

happyReduce_146 = happyReduce 6# 91# happyReduction_146
happyReduction_146 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	case happyOut82 happy_x_2 of { (HappyWrap82 happy_var_2) -> 
	case happyOut14 happy_x_3 of { (HappyWrap14 happy_var_3) -> 
	case happyOut95 happy_x_4 of { (HappyWrap95 happy_var_4) -> 
	case happyOut15 happy_x_5 of { (HappyWrap15 happy_var_5) -> 
	case happyOut95 happy_x_6 of { (HappyWrap95 happy_var_6) -> 
	happyIn95
		 (AbsChapel.EifExp happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}}

happyReduce_147 = happySpecReduce_1  91# happyReduction_147
happyReduction_147 happy_x_1
	 =  case happyOut96 happy_x_1 of { (HappyWrap96 happy_var_1) -> 
	happyIn95
		 (happy_var_1
	)}

happyReduce_148 = happySpecReduce_3  92# happyReduction_148
happyReduction_148 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut86 happy_x_2 of { (HappyWrap86 happy_var_2) -> 
	case happyOut7 happy_x_3 of { (HappyWrap7 happy_var_3) -> 
	happyIn96
		 (AbsChapel.InnerExp happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_149 = happyReduce 4# 92# happyReduction_149
happyReduction_149 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut6 happy_x_2 of { (HappyWrap6 happy_var_2) -> 
	case happyOut76 happy_x_3 of { (HappyWrap76 happy_var_3) -> 
	case happyOut7 happy_x_4 of { (HappyWrap7 happy_var_4) -> 
	happyIn96
		 (AbsChapel.EFun happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_150 = happySpecReduce_1  92# happyReduction_150
happyReduction_150 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn96
		 (AbsChapel.Evar happy_var_1
	)}

happyReduce_151 = happySpecReduce_1  92# happyReduction_151
happyReduction_151 happy_x_1
	 =  case happyOut98 happy_x_1 of { (HappyWrap98 happy_var_1) -> 
	happyIn96
		 (AbsChapel.Econst happy_var_1
	)}

happyReduce_152 = happySpecReduce_1  93# happyReduction_152
happyReduction_152 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn97
		 (AbsChapel.Negation happy_var_1
	)}

happyReduce_153 = happySpecReduce_1  93# happyReduction_153
happyReduction_153 happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	happyIn97
		 (AbsChapel.MinusUnary happy_var_1
	)}

happyReduce_154 = happySpecReduce_1  93# happyReduction_154
happyReduction_154 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn97
		 (AbsChapel.Address happy_var_1
	)}

happyReduce_155 = happySpecReduce_1  93# happyReduction_155
happyReduction_155 happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	happyIn97
		 (AbsChapel.Indirection happy_var_1
	)}

happyReduce_156 = happySpecReduce_1  94# happyReduction_156
happyReduction_156 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn98
		 (AbsChapel.Estring happy_var_1
	)}

happyReduce_157 = happySpecReduce_1  94# happyReduction_157
happyReduction_157 happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	happyIn98
		 (AbsChapel.Efloat happy_var_1
	)}

happyReduce_158 = happySpecReduce_1  94# happyReduction_158
happyReduction_158 happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn98
		 (AbsChapel.Echar happy_var_1
	)}

happyReduce_159 = happySpecReduce_1  94# happyReduction_159
happyReduction_159 happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn98
		 (AbsChapel.Eint happy_var_1
	)}

happyReduce_160 = happySpecReduce_1  94# happyReduction_160
happyReduction_160 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn98
		 (AbsChapel.ETrue happy_var_1
	)}

happyReduce_161 = happySpecReduce_1  94# happyReduction_161
happyReduction_161 happy_x_1
	 =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	happyIn98
		 (AbsChapel.EFalse happy_var_1
	)}

happyNewToken action sts stk [] =
	happyDoAction 52# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (T_POpenGraph _) -> cont 2#;
	PT _ (T_PCloseGraph _) -> cont 3#;
	PT _ (T_POpenParenthesis _) -> cont 4#;
	PT _ (T_PCloseParenthesis _) -> cont 5#;
	PT _ (T_POpenBracket _) -> cont 6#;
	PT _ (T_PCloseBracket _) -> cont 7#;
	PT _ (T_PSemicolon _) -> cont 8#;
	PT _ (T_PColon _) -> cont 9#;
	PT _ (T_PPoint _) -> cont 10#;
	PT _ (T_PIf _) -> cont 11#;
	PT _ (T_PThen _) -> cont 12#;
	PT _ (T_PElse _) -> cont 13#;
	PT _ (T_Pdo _) -> cont 14#;
	PT _ (T_PWhile _) -> cont 15#;
	PT _ (T_PIntType _) -> cont 16#;
	PT _ (T_PRealType _) -> cont 17#;
	PT _ (T_PCharType _) -> cont 18#;
	PT _ (T_PBoolType _) -> cont 19#;
	PT _ (T_PStringType _) -> cont 20#;
	PT _ (T_PBreak _) -> cont 21#;
	PT _ (T_PContinue _) -> cont 22#;
	PT _ (T_PAssignmEq _) -> cont 23#;
	PT _ (T_PRef _) -> cont 24#;
	PT _ (T_PVar _) -> cont 25#;
	PT _ (T_PProc _) -> cont 26#;
	PT _ (T_PReturn _) -> cont 27#;
	PT _ (T_PTrue _) -> cont 28#;
	PT _ (T_PFalse _) -> cont 29#;
	PT _ (T_PEpow _) -> cont 30#;
	PT _ (T_PElthen _) -> cont 31#;
	PT _ (T_PEgrthen _) -> cont 32#;
	PT _ (T_PEplus _) -> cont 33#;
	PT _ (T_PEminus _) -> cont 34#;
	PT _ (T_PEtimes _) -> cont 35#;
	PT _ (T_PEdiv _) -> cont 36#;
	PT _ (T_PEmod _) -> cont 37#;
	PT _ (T_PDef _) -> cont 38#;
	PT _ (T_PNeg _) -> cont 39#;
	PT _ (T_PElor _) -> cont 40#;
	PT _ (T_PEland _) -> cont 41#;
	PT _ (T_PEeq _) -> cont 42#;
	PT _ (T_PEneq _) -> cont 43#;
	PT _ (T_PEle _) -> cont 44#;
	PT _ (T_PEge _) -> cont 45#;
	PT _ (T_PAssignmPlus _) -> cont 46#;
	PT _ (T_PIdent _) -> cont 47#;
	PT _ (T_PString _) -> cont 48#;
	PT _ (T_PChar _) -> cont 49#;
	PT _ (T_PDouble _) -> cont 50#;
	PT _ (T_PInteger _) -> cont 51#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 52# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap54 x') = happyOut54 x} in x'))

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
