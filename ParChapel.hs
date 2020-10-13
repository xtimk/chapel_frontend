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
newtype HappyWrap18 = HappyWrap18 (PInt)
happyIn18 :: (PInt) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 (PReal)
happyIn19 :: (PReal) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 (PAssignmEq)
happyIn20 :: (PAssignmEq) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 (PAssignmPlus)
happyIn21 :: (PAssignmPlus) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (PRef)
happyIn22 :: (PRef) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 (PVar)
happyIn23 :: (PVar) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 (PConst)
happyIn24 :: (PConst) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 (PProc)
happyIn25 :: (PProc) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 (PReturn)
happyIn26 :: (PReturn) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 (PElthen)
happyIn27 :: (PElthen) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 (PEgrthen)
happyIn28 :: (PEgrthen) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 (PEplus)
happyIn29 :: (PEplus) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 (PEminus)
happyIn30 :: (PEminus) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 (PEtimes)
happyIn31 :: (PEtimes) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 (PEdiv)
happyIn32 :: (PEdiv) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 (PEmod)
happyIn33 :: (PEmod) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 (PDef)
happyIn34 :: (PDef) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 (PElor)
happyIn35 :: (PElor) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 (PEland)
happyIn36 :: (PEland) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 (PEeq)
happyIn37 :: (PEeq) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 (PEneq)
happyIn38 :: (PEneq) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 (PEle)
happyIn39 :: (PEle) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 (PEge)
happyIn40 :: (PEge) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 (PIdent)
happyIn41 :: (PIdent) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 (PString)
happyIn42 :: (PString) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 (PChar)
happyIn43 :: (PChar) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 (PDouble)
happyIn44 :: (PDouble) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 (PInteger)
happyIn45 :: (PInteger) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 (Program)
happyIn46 :: (Program) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 (Module)
happyIn47 :: (Module) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 ([Ext])
happyIn48 :: ([Ext]) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 (Ext)
happyIn49 :: (Ext) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 (Declaration)
happyIn50 :: (Declaration) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 ([DeclList])
happyIn51 :: ([DeclList]) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 (DeclList)
happyIn52 :: (DeclList) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 (ArDecl)
happyIn53 :: (ArDecl) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 (ArDim)
happyIn54 :: (ArDim) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 ([ArDim])
happyIn55 :: ([ArDim]) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 ([ArBound])
happyIn56 :: ([ArBound]) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 (ArBound)
happyIn57 :: (ArBound) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 ([PIdent])
happyIn58 :: ([PIdent]) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 (DecMode)
happyIn59 :: (DecMode) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
newtype HappyWrap60 = HappyWrap60 (Function)
happyIn60 :: (Function) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap60 x)
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> HappyWrap60
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
newtype HappyWrap61 = HappyWrap61 (Signature)
happyIn61 :: (Signature) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap61 x)
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> HappyWrap61
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
newtype HappyWrap62 = HappyWrap62 (FunctionParams)
happyIn62 :: (FunctionParams) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap62 x)
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> HappyWrap62
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
newtype HappyWrap63 = HappyWrap63 ([Param])
happyIn63 :: ([Param]) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap63 x)
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> HappyWrap63
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
newtype HappyWrap64 = HappyWrap64 (Param)
happyIn64 :: (Param) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap64 x)
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> HappyWrap64
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
newtype HappyWrap65 = HappyWrap65 (Body)
happyIn65 :: (Body) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap65 x)
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> HappyWrap65
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
newtype HappyWrap66 = HappyWrap66 ([BodyStatement])
happyIn66 :: ([BodyStatement]) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap66 x)
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> HappyWrap66
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
newtype HappyWrap67 = HappyWrap67 (BodyStatement)
happyIn67 :: (BodyStatement) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap67 x)
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> HappyWrap67
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
newtype HappyWrap68 = HappyWrap68 (Statement)
happyIn68 :: (Statement) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap68 x)
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> HappyWrap68
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
newtype HappyWrap69 = HappyWrap69 (Guard)
happyIn69 :: (Guard) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap69 x)
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> HappyWrap69
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
newtype HappyWrap70 = HappyWrap70 (Type)
happyIn70 :: (Type) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap70 x)
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> HappyWrap70
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
newtype HappyWrap71 = HappyWrap71 (AssgnmOp)
happyIn71 :: (AssgnmOp) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap71 x)
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> HappyWrap71
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
newtype HappyWrap72 = HappyWrap72 (Mode)
happyIn72 :: (Mode) -> (HappyAbsSyn )
happyIn72 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap72 x)
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> HappyWrap72
happyOut72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut72 #-}
newtype HappyWrap73 = HappyWrap73 (Exp)
happyIn73 :: (Exp) -> (HappyAbsSyn )
happyIn73 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap73 x)
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> HappyWrap73
happyOut73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut73 #-}
newtype HappyWrap74 = HappyWrap74 (Exp)
happyIn74 :: (Exp) -> (HappyAbsSyn )
happyIn74 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap74 x)
{-# INLINE happyIn74 #-}
happyOut74 :: (HappyAbsSyn ) -> HappyWrap74
happyOut74 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut74 #-}
newtype HappyWrap75 = HappyWrap75 (Exp)
happyIn75 :: (Exp) -> (HappyAbsSyn )
happyIn75 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap75 x)
{-# INLINE happyIn75 #-}
happyOut75 :: (HappyAbsSyn ) -> HappyWrap75
happyOut75 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut75 #-}
newtype HappyWrap76 = HappyWrap76 (Exp)
happyIn76 :: (Exp) -> (HappyAbsSyn )
happyIn76 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap76 x)
{-# INLINE happyIn76 #-}
happyOut76 :: (HappyAbsSyn ) -> HappyWrap76
happyOut76 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut76 #-}
newtype HappyWrap77 = HappyWrap77 (Exp)
happyIn77 :: (Exp) -> (HappyAbsSyn )
happyIn77 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap77 x)
{-# INLINE happyIn77 #-}
happyOut77 :: (HappyAbsSyn ) -> HappyWrap77
happyOut77 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut77 #-}
newtype HappyWrap78 = HappyWrap78 (Exp)
happyIn78 :: (Exp) -> (HappyAbsSyn )
happyIn78 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap78 x)
{-# INLINE happyIn78 #-}
happyOut78 :: (HappyAbsSyn ) -> HappyWrap78
happyOut78 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut78 #-}
newtype HappyWrap79 = HappyWrap79 (Exp)
happyIn79 :: (Exp) -> (HappyAbsSyn )
happyIn79 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap79 x)
{-# INLINE happyIn79 #-}
happyOut79 :: (HappyAbsSyn ) -> HappyWrap79
happyOut79 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut79 #-}
newtype HappyWrap80 = HappyWrap80 (Exp)
happyIn80 :: (Exp) -> (HappyAbsSyn )
happyIn80 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap80 x)
{-# INLINE happyIn80 #-}
happyOut80 :: (HappyAbsSyn ) -> HappyWrap80
happyOut80 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut80 #-}
newtype HappyWrap81 = HappyWrap81 (Exp)
happyIn81 :: (Exp) -> (HappyAbsSyn )
happyIn81 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap81 x)
{-# INLINE happyIn81 #-}
happyOut81 :: (HappyAbsSyn ) -> HappyWrap81
happyOut81 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut81 #-}
newtype HappyWrap82 = HappyWrap82 (Exp)
happyIn82 :: (Exp) -> (HappyAbsSyn )
happyIn82 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap82 x)
{-# INLINE happyIn82 #-}
happyOut82 :: (HappyAbsSyn ) -> HappyWrap82
happyOut82 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut82 #-}
newtype HappyWrap83 = HappyWrap83 (Exp)
happyIn83 :: (Exp) -> (HappyAbsSyn )
happyIn83 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap83 x)
{-# INLINE happyIn83 #-}
happyOut83 :: (HappyAbsSyn ) -> HappyWrap83
happyOut83 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut83 #-}
newtype HappyWrap84 = HappyWrap84 (Exp)
happyIn84 :: (Exp) -> (HappyAbsSyn )
happyIn84 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap84 x)
{-# INLINE happyIn84 #-}
happyOut84 :: (HappyAbsSyn ) -> HappyWrap84
happyOut84 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut84 #-}
newtype HappyWrap85 = HappyWrap85 (Constant)
happyIn85 :: (Constant) -> (HappyAbsSyn )
happyIn85 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap85 x)
{-# INLINE happyIn85 #-}
happyOut85 :: (HappyAbsSyn ) -> HappyWrap85
happyOut85 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut85 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\xc8\xe0\x01\x80\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x80\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\xe0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\xf8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","POpenGraph","PCloseGraph","POpenParenthesis","PCloseParenthesis","POpenBracket","PCloseBracket","PSemicolon","PColon","PPoint","PIf","PThen","PElse","Pdo","PWhile","PInt","PReal","PAssignmEq","PAssignmPlus","PRef","PVar","PConst","PProc","PReturn","PElthen","PEgrthen","PEplus","PEminus","PEtimes","PEdiv","PEmod","PDef","PElor","PEland","PEeq","PEneq","PEle","PEge","PIdent","PString","PChar","PDouble","PInteger","Program","Module","ListExt","Ext","Declaration","ListDeclList","DeclList","ArDecl","ArDim","ListArDim","ListArBound","ArBound","ListPIdent","DecMode","Function","Signature","FunctionParams","ListParam","Param","Body","ListBodyStatement","BodyStatement","Statement","Guard","Type","AssgnmOp","Mode","Exp","Exp4","Exp5","Exp8","Exp9","Exp10","Exp12","Exp13","Exp6","Exp7","Exp11","Exp14","Constant","','","L_POpenGraph","L_PCloseGraph","L_POpenParenthesis","L_PCloseParenthesis","L_POpenBracket","L_PCloseBracket","L_PSemicolon","L_PColon","L_PPoint","L_PIf","L_PThen","L_PElse","L_Pdo","L_PWhile","L_PInt","L_PReal","L_PAssignmEq","L_PAssignmPlus","L_PRef","L_PVar","L_PConst","L_PProc","L_PReturn","L_PElthen","L_PEgrthen","L_PEplus","L_PEminus","L_PEtimes","L_PEdiv","L_PEmod","L_PDef","L_PElor","L_PEland","L_PEeq","L_PEneq","L_PEle","L_PEge","L_PIdent","L_PString","L_PChar","L_PDouble","L_PInteger","%eof"]
        bit_start = st * 129
        bit_end = (st + 1) * 129
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..128]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x0f\x00\x00\x00\xdc\xff\x00\x00\x7e\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x2f\x00\x39\x00\x24\x00\x00\x00\x69\x00\x6f\x00\x00\x00\x00\x00\xf5\xff\x8d\x00\x00\x00\x98\x00\x34\x02\x00\x00\x00\x00\x79\x00\x00\x00\x00\x00\x79\x00\x00\x00\x00\x00\x34\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x86\x00\x88\x00\x9f\x00\xdf\x00\xb3\x00\x0b\x01\xb5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x01\x00\x00\x00\x00\x20\x01\x9a\x00\x00\x00\x00\x00\x00\x00\x20\x01\x00\x00\xbd\x00\xd0\x00\xd6\x00\xd5\x00\x00\x00\x2f\x02\x00\x00\x0e\x01\xf8\x00\x26\x01\x99\x00\x00\x00\x3a\x01\x00\x00\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4b\x01\xf5\xff\x00\x00\x00\x00\x20\x01\x00\x00\x34\x02\x00\x00\x00\x00\x74\x01\x56\x01\x56\x01\xa1\x00\x00\x00\x34\x02\x34\x02\x34\x02\x00\x00\x00\x00\x00\x00\x34\x02\x34\x02\x00\x00\x00\x00\x34\x02\x34\x02\x34\x02\x34\x02\x00\x00\x00\x00\x00\x00\x00\x00\x34\x02\x34\x02\x00\x00\x00\x00\x34\x02\x00\x00\x34\x02\x00\x00\x34\x02\x00\x00\x00\x00\x00\x00\x34\x02\x00\x00\x93\x00\x00\x00\x49\x01\x4a\x01\x6a\x01\x43\x01\xb3\x00\xb3\x00\x4d\x01\x00\x00\x00\x00\x00\x00\xb5\x00\xb5\x00\x00\x00\x00\x00\x00\x00\x67\x01\x16\x01\x00\x00\x00\x00\x00\x00\x00\x00\x16\x01\x5b\x01\x00\x00\x00\x00\x5f\x01\x00\x00\x00\x00\x00\x00\x01\x01\x34\x02\x91\x01\x91\x01\x88\x01\x98\x01\x00\x00\x9a\x01\x00\x00\x93\x00\x00\x00\x00\x00\x00\x00\x99\x01\x00\x00\xa3\x01\x16\x01\x00\x00\x00\x00\x00\x00\x9b\x01\xaf\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xe4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x02\x00\x00\x00\x00\xdf\xff\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xac\x01\x00\x00\xf4\x00\x00\x00\x21\x00\x06\x00\x75\x01\x00\x00\xcd\x01\xb0\x01\x00\x00\x61\x00\x3b\x00\x00\x00\x00\x00\x03\x01\x00\x00\x00\x00\xf8\xff\x00\x00\x00\x00\x68\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\x9f\x01\xa7\x01\xaa\x01\x52\x01\xe2\x00\x62\x01\x1f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\x00\x00\x00\x00\x00\xfd\xff\xb9\x01\x00\x00\x00\x00\x00\x00\x5a\x00\x00\x00\xc8\x01\xce\x01\x00\x00\xfc\xff\x00\x00\x01\x00\x00\x00\x17\x00\xc3\x01\x18\x00\x2e\x00\x00\x00\xcf\x01\x00\x00\x00\x00\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\x01\xd8\x01\x00\x00\x00\x00\x5d\x00\x00\x00\x95\x00\x00\x00\x00\x00\x00\x00\xd1\x01\xe0\x01\xde\x01\x00\x00\xb4\x01\xbb\x01\xd2\x01\x00\x00\x00\x00\x00\x00\x9d\x01\xa5\x01\x00\x00\x00\x00\x5a\x01\x69\x01\x70\x01\x87\x01\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x01\x51\x01\x00\x00\x00\x00\x0f\x01\x00\x00\x06\x01\x00\x00\xfc\x00\x00\x00\x00\x00\x00\x00\xcf\x00\x00\x00\x02\x00\x00\x00\xc9\x01\xc7\x01\xd5\x01\x5c\x01\xe2\x00\xe2\x00\x6b\x01\x00\x00\x00\x00\x00\x00\x1f\x01\x1f\x01\x00\x00\x00\x00\x00\x00\xe3\x01\xf8\x01\x00\x00\x00\x00\x00\x00\x00\x00\xf2\x01\xff\xff\x00\x00\x00\x00\x8c\x00\x00\x00\x00\x00\x00\x00\x56\x00\xa2\x00\x07\x00\x20\x00\xe2\x01\x22\x00\x00\x00\x19\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x00\x00\xe7\x01\x00\x00\x00\x00\x01\x02\x00\x00\x00\x00\x00\x00\xe5\x01\x35\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xd2\xff\x00\x00\xfe\xff\x00\x00\xd4\xff\xd3\xff\xbb\xff\xba\xff\x00\x00\xd1\xff\xd0\xff\x00\x00\xcf\xff\xeb\xff\xea\xff\xe9\xff\xbd\xff\x00\x00\xcd\xff\x00\x00\xd9\xff\x00\x00\x00\x00\xaf\xff\xb9\xff\xb5\xff\xb8\xff\xfc\xff\x00\x00\x00\x00\xf7\xff\xee\xff\x00\x00\xce\xff\xf8\xff\x00\x00\xbc\xff\xcc\xff\x00\x00\x81\xff\x7f\xff\x7d\xff\x7e\xff\x7c\xff\xc8\xff\x9b\xff\x99\xff\x84\xff\x95\xff\x92\xff\x83\xff\x8a\xff\x97\xff\x85\xff\x8d\xff\x86\xff\x80\xff\xd8\xff\xd7\xff\xd6\xff\xd5\xff\x00\x00\xa1\xff\xa0\xff\xca\xff\xcb\xff\xfa\xff\xf0\xff\xef\xff\x00\x00\x9d\xff\x00\x00\x00\x00\xb4\xff\x00\x00\xec\xff\x00\x00\xb0\xff\x00\x00\x00\x00\x00\x00\x00\x00\xab\xff\x00\x00\xaa\xff\xae\xff\xad\xff\x00\x00\xfd\xff\xf5\xff\xf2\xff\xf1\xff\xe8\xff\x00\x00\xb5\xff\xb6\xff\xfb\xff\x00\x00\xb7\xff\x00\x00\xc9\xff\xbf\xff\xc3\xff\x00\x00\x00\x00\xc1\xff\xbe\xff\x00\x00\x00\x00\x00\x00\xe3\xff\xe2\xff\xe1\xff\x00\x00\x00\x00\xe5\xff\xe4\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe7\xff\xe6\xff\xdb\xff\xda\xff\x00\x00\x00\x00\xdd\xff\xdc\xff\x00\x00\xe0\xff\x00\x00\xde\xff\x00\x00\xdf\xff\x9f\xff\x9e\xff\x00\x00\xed\xff\x00\x00\x82\xff\x9c\xff\x9a\xff\x98\xff\x96\xff\x93\xff\x94\xff\x8e\xff\x8f\xff\x90\xff\x91\xff\x8b\xff\x8c\xff\x87\xff\x88\xff\x89\xff\x00\x00\x00\x00\xf6\xff\xc5\xff\xf9\xff\xc6\xff\x00\x00\xc7\xff\xb2\xff\xb3\xff\x00\x00\xa3\xff\xac\xff\xa8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\x00\x00\xa6\xff\x00\x00\xa9\xff\xb1\xff\xc2\xff\x00\x00\xc0\xff\xc1\xff\x00\x00\xc4\xff\xa2\xff\xa7\xff\xa5\xff\x00\x00\xf3\xff\xa4\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x00\x00\x01\x00\x02\x00\x25\x00\x03\x00\x00\x00\x00\x00\x2c\x00\x14\x00\x09\x00\x0e\x00\x0f\x00\x0c\x00\x0d\x00\x10\x00\x11\x00\x02\x00\x10\x00\x11\x00\x13\x00\x14\x00\x15\x00\x16\x00\x39\x00\x02\x00\x02\x00\x02\x00\x27\x00\x25\x00\x03\x00\x01\x00\x00\x00\x25\x00\x00\x00\x02\x00\x12\x00\x13\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x10\x00\x11\x00\x09\x00\x36\x00\x2e\x00\x02\x00\x06\x00\x36\x00\x27\x00\x06\x00\x00\x00\x12\x00\x08\x00\x37\x00\x38\x00\x01\x00\x10\x00\x11\x00\x02\x00\x3d\x00\x42\x00\x3f\x00\x40\x00\x43\x00\x3d\x00\x3d\x00\x43\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x41\x00\x41\x00\x41\x00\x3a\x00\x06\x00\x3d\x00\x43\x00\x3d\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x04\x00\x10\x00\x11\x00\x0e\x00\x0f\x00\x02\x00\x0e\x00\x0f\x00\x04\x00\x43\x00\x0e\x00\x0f\x00\x02\x00\x3d\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x31\x00\x15\x00\x16\x00\x17\x00\x09\x00\x02\x00\x05\x00\x43\x00\x0e\x00\x0f\x00\x42\x00\x04\x00\x06\x00\x42\x00\x27\x00\x08\x00\x01\x00\x42\x00\x02\x00\x12\x00\x13\x00\x21\x00\x10\x00\x11\x00\x22\x00\x0a\x00\x12\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x20\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x25\x00\x09\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x19\x00\x1a\x00\x42\x00\x2f\x00\x30\x00\x02\x00\x1d\x00\x1e\x00\x1f\x00\x05\x00\x36\x00\x01\x00\x25\x00\x26\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x17\x00\x18\x00\x07\x00\x27\x00\x25\x00\x02\x00\x27\x00\x28\x00\x29\x00\x23\x00\x24\x00\x10\x00\x23\x00\x24\x00\x0f\x00\x02\x00\x08\x00\x32\x00\x33\x00\x34\x00\x35\x00\x2a\x00\x2b\x00\x2c\x00\x02\x00\x04\x00\x12\x00\x13\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x1b\x00\x1c\x00\x25\x00\x51\x00\x04\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x10\x00\x11\x00\x2f\x00\x30\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x36\x00\x1b\x00\x1c\x00\x1d\x00\x27\x00\x02\x00\x29\x00\x2a\x00\x2b\x00\x08\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x02\x00\x09\x00\x4f\x00\x50\x00\x51\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x02\x00\x07\x00\x4f\x00\x50\x00\x51\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x23\x00\x24\x00\x1b\x00\x1c\x00\x21\x00\x02\x00\x22\x00\x12\x00\x13\x00\x10\x00\x11\x00\x0a\x00\x02\x00\x21\x00\x22\x00\x01\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x19\x00\x1a\x00\x21\x00\x22\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x19\x00\x1a\x00\x4a\x00\x4b\x00\x4c\x00\x02\x00\x20\x00\x4f\x00\x50\x00\x51\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x02\x00\x0c\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x02\x00\x4a\x00\x4b\x00\x4c\x00\x04\x00\x02\x00\x4f\x00\x50\x00\x51\x00\x0a\x00\x01\x00\x4b\x00\x4c\x00\x02\x00\x0d\x00\x4f\x00\x50\x00\x51\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x02\x00\x06\x00\x3e\x00\x4b\x00\x4c\x00\x02\x00\x07\x00\x4f\x00\x50\x00\x51\x00\x4b\x00\x4c\x00\x02\x00\x1f\x00\x4f\x00\x50\x00\x51\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x20\x00\x1e\x00\x10\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x07\x00\x0d\x00\x03\x00\x4b\x00\x4c\x00\x02\x00\x06\x00\x05\x00\x50\x00\x51\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x07\x00\x12\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x05\x00\x08\x00\x20\x00\x1f\x00\x4c\x00\x12\x00\x08\x00\x0a\x00\x50\x00\x51\x00\x08\x00\x0b\x00\x4c\x00\x25\x00\x1e\x00\xff\xff\x50\x00\x51\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x36\x00\x50\x00\x51\x00\xff\xff\xff\xff\x3b\x00\x3c\x00\xff\xff\x50\x00\x51\x00\xff\xff\x36\x00\xff\xff\xff\xff\x44\x00\xff\xff\x3b\x00\x3c\x00\xff\xff\xff\xff\x25\x00\xff\xff\x27\x00\x28\x00\x29\x00\x44\x00\x25\x00\xff\xff\x27\x00\x28\x00\x29\x00\x50\x00\x51\x00\x32\x00\x33\x00\x25\x00\x35\x00\x27\x00\x28\x00\x29\x00\xff\xff\x34\x00\x35\x00\xff\xff\xff\xff\xff\xff\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\x35\x00\xff\xff\x04\x00\xff\xff\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\xff\xff\xff\xff\xff\xff\xff\xff\x51\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x51\x00\x13\x00\x14\x00\x15\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x51\x00\xff\xff\xff\xff\xff\xff\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x37\x00\x38\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x17\x00\x4d\x00\x26\x00\x15\x00\x8c\x00\x17\x00\x17\x00\xff\xff\x4c\x00\x4e\x00\x3e\x00\x3f\x00\x4f\x00\x50\x00\x87\x00\x88\x00\x03\x00\x87\x00\x88\x00\x06\x00\x07\x00\x08\x00\x51\x00\x16\x00\xab\x00\xab\x00\xab\x00\x15\x00\x10\x00\xbc\x00\x24\x00\x17\x00\x10\x00\x17\x00\x19\x00\x20\x00\x8b\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x87\x00\x88\x00\x1f\x00\x24\x00\x52\x00\x26\x00\xa7\x00\x5d\x00\x15\x00\xa9\x00\x17\x00\x20\x00\x23\x00\x0b\x00\x53\x00\x21\x00\x87\x00\x88\x00\x26\x00\x54\x00\x64\x00\x55\x00\x56\x00\x89\x00\x18\x00\xb2\x00\x89\x00\x57\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\xae\x00\xac\x00\xbd\x00\x1a\x00\xb4\x00\xb1\x00\x89\x00\xbe\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x3d\x00\x87\x00\x88\x00\x3e\x00\x3f\x00\x26\x00\x3e\x00\x3f\x00\x1c\x00\x89\x00\x3e\x00\x3f\x00\x03\x00\xc1\x00\xaa\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x40\x00\x0e\x00\x0f\x00\x10\x00\x1f\x00\x26\x00\x61\x00\x89\x00\x3e\x00\x3f\x00\x62\x00\x1c\x00\x43\x00\xa4\x00\x15\x00\x23\x00\x9e\x00\x41\x00\x26\x00\x20\x00\x8b\x00\x87\x00\x44\x00\x45\x00\x85\x00\x9f\x00\x20\x00\x8b\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x83\x00\x15\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x10\x00\x1f\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x7a\x00\x7b\x00\xb5\x00\x11\x00\x12\x00\x26\x00\x6f\x00\x70\x00\x71\x00\x61\x00\x13\x00\x5f\x00\x7c\x00\x7d\x00\xa3\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\xb3\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x75\x00\x76\x00\x1c\x00\x15\x00\x65\x00\x26\x00\x29\x00\x2a\x00\x2b\x00\x80\x00\x81\x00\x1d\x00\x77\x00\x78\x00\x5c\x00\x26\x00\x23\x00\x66\x00\x67\x00\x68\x00\x69\x00\x03\x00\x04\x00\x05\x00\x26\x00\x1c\x00\x20\x00\x8b\x00\x8d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x74\x00\x75\x00\x10\x00\x6a\x00\x1c\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x44\x00\x45\x00\x25\x00\x12\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x13\x00\x6b\x00\x6c\x00\x6d\x00\x15\x00\x26\x00\x3b\x00\x3c\x00\x3d\x00\x23\x00\x8e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x8f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x26\x00\x1f\x00\x36\x00\x37\x00\x38\x00\x90\x00\x31\x00\x32\x00\x33\x00\x26\x00\xa1\x00\x36\x00\x37\x00\x38\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x80\x00\x81\x00\x74\x00\x75\x00\x87\x00\x26\x00\x85\x00\x20\x00\x8b\x00\x44\x00\x45\x00\x9f\x00\x26\x00\x7d\x00\x7e\x00\xa3\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x71\x00\x72\x00\x7d\x00\x7e\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x71\x00\x72\x00\x92\x00\x32\x00\x33\x00\x26\x00\x83\x00\x36\x00\x37\x00\x38\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x03\x00\xb1\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x03\x00\x91\x00\x32\x00\x33\x00\x1c\x00\x26\x00\x36\x00\x37\x00\x38\x00\x9f\x00\x9e\x00\x32\x00\x33\x00\x26\x00\xc1\x00\x96\x00\x37\x00\x38\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x03\x00\x21\x00\x4c\x00\x32\x00\x33\x00\x26\x00\x45\x00\x95\x00\x37\x00\x38\x00\x32\x00\x33\x00\x26\x00\x85\x00\x94\x00\x37\x00\x38\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x83\x00\x81\x00\x63\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x61\x00\xad\x00\x5f\x00\x93\x00\x33\x00\x26\x00\xa8\x00\xa1\x00\x37\x00\x38\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\xa6\x00\x46\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x9f\x00\x9c\x00\x83\x00\x85\x00\x98\x00\x46\x00\xba\x00\xaf\x00\x37\x00\x38\x00\x9c\x00\xbf\x00\x97\x00\x10\x00\x81\x00\x00\x00\x37\x00\x38\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x9b\x00\x38\x00\x00\x00\x00\x00\x48\x00\x49\x00\x00\x00\x9a\x00\x38\x00\x00\x00\x47\x00\x00\x00\x00\x00\x4a\x00\x00\x00\xa5\x00\x49\x00\x00\x00\x00\x00\x65\x00\x00\x00\x29\x00\x2a\x00\x2b\x00\x4a\x00\x65\x00\x00\x00\x29\x00\x2a\x00\x2b\x00\x99\x00\x38\x00\x66\x00\xb6\x00\x65\x00\xb7\x00\x29\x00\x2a\x00\x2b\x00\x00\x00\xb8\x00\xb9\x00\x00\x00\x00\x00\x00\x00\x03\x00\x59\x00\x1c\x00\x00\x00\x00\x00\xbb\x00\x00\x00\x1c\x00\x00\x00\x5a\x00\x00\x00\x00\x00\x5b\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x0e\x00\x0f\x00\x10\x00\x5d\x00\x00\x00\x6a\x00\x06\x00\x07\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x15\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x15\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 131) [
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
	(131 , happyReduce_131)
	]

happy_n_terms = 45 :: Int
happy_n_nonterms = 82 :: Int

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
		 (PInt (mkPosToken happy_var_1)
	)}

happyReduce_16 = happySpecReduce_1  15# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (PReal (mkPosToken happy_var_1)
	)}

happyReduce_17 = happySpecReduce_1  16# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (PAssignmEq (mkPosToken happy_var_1)
	)}

happyReduce_18 = happySpecReduce_1  17# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (PAssignmPlus (mkPosToken happy_var_1)
	)}

happyReduce_19 = happySpecReduce_1  18# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (PRef (mkPosToken happy_var_1)
	)}

happyReduce_20 = happySpecReduce_1  19# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (PVar (mkPosToken happy_var_1)
	)}

happyReduce_21 = happySpecReduce_1  20# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (PConst (mkPosToken happy_var_1)
	)}

happyReduce_22 = happySpecReduce_1  21# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (PProc (mkPosToken happy_var_1)
	)}

happyReduce_23 = happySpecReduce_1  22# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (PReturn (mkPosToken happy_var_1)
	)}

happyReduce_24 = happySpecReduce_1  23# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (PElthen (mkPosToken happy_var_1)
	)}

happyReduce_25 = happySpecReduce_1  24# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (PEgrthen (mkPosToken happy_var_1)
	)}

happyReduce_26 = happySpecReduce_1  25# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (PEplus (mkPosToken happy_var_1)
	)}

happyReduce_27 = happySpecReduce_1  26# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (PEminus (mkPosToken happy_var_1)
	)}

happyReduce_28 = happySpecReduce_1  27# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (PEtimes (mkPosToken happy_var_1)
	)}

happyReduce_29 = happySpecReduce_1  28# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (PEdiv (mkPosToken happy_var_1)
	)}

happyReduce_30 = happySpecReduce_1  29# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (PEmod (mkPosToken happy_var_1)
	)}

happyReduce_31 = happySpecReduce_1  30# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (PDef (mkPosToken happy_var_1)
	)}

happyReduce_32 = happySpecReduce_1  31# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (PElor (mkPosToken happy_var_1)
	)}

happyReduce_33 = happySpecReduce_1  32# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (PEland (mkPosToken happy_var_1)
	)}

happyReduce_34 = happySpecReduce_1  33# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (PEeq (mkPosToken happy_var_1)
	)}

happyReduce_35 = happySpecReduce_1  34# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (PEneq (mkPosToken happy_var_1)
	)}

happyReduce_36 = happySpecReduce_1  35# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (PEle (mkPosToken happy_var_1)
	)}

happyReduce_37 = happySpecReduce_1  36# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (PEge (mkPosToken happy_var_1)
	)}

happyReduce_38 = happySpecReduce_1  37# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (PIdent (mkPosToken happy_var_1)
	)}

happyReduce_39 = happySpecReduce_1  38# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (PString (mkPosToken happy_var_1)
	)}

happyReduce_40 = happySpecReduce_1  39# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (PChar (mkPosToken happy_var_1)
	)}

happyReduce_41 = happySpecReduce_1  40# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (PDouble (mkPosToken happy_var_1)
	)}

happyReduce_42 = happySpecReduce_1  41# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (PInteger (mkPosToken happy_var_1)
	)}

happyReduce_43 = happySpecReduce_1  42# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn46
		 (AbsChapel.Progr happy_var_1
	)}

happyReduce_44 = happySpecReduce_1  43# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn47
		 (AbsChapel.Mod (reverse happy_var_1)
	)}

happyReduce_45 = happySpecReduce_0  44# happyReduction_45
happyReduction_45  =  happyIn48
		 ([]
	)

happyReduce_46 = happySpecReduce_2  44# happyReduction_46
happyReduction_46 happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut49 happy_x_2 of { (HappyWrap49 happy_var_2) -> 
	happyIn48
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_47 = happySpecReduce_1  45# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn49
		 (AbsChapel.ExtDecl happy_var_1
	)}

happyReduce_48 = happySpecReduce_1  45# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut60 happy_x_1 of { (HappyWrap60 happy_var_1) -> 
	happyIn49
		 (AbsChapel.ExtFun happy_var_1
	)}

happyReduce_49 = happySpecReduce_3  46# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_1 of { (HappyWrap59 happy_var_1) -> 
	case happyOut51 happy_x_2 of { (HappyWrap51 happy_var_2) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn50
		 (AbsChapel.Decl happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_50 = happySpecReduce_1  47# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	happyIn51
		 ((:[]) happy_var_1
	)}

happyReduce_51 = happySpecReduce_3  47# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut51 happy_x_3 of { (HappyWrap51 happy_var_3) -> 
	happyIn51
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_52 = happySpecReduce_3  48# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	case happyOut70 happy_x_3 of { (HappyWrap70 happy_var_3) -> 
	happyIn52
		 (AbsChapel.NoAssgmDec happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_53 = happySpecReduce_3  48# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn52
		 (AbsChapel.NoAssgmArrayFixDec happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_54 = happyReduce 4# 48# happyReduction_54
happyReduction_54 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	case happyOut70 happy_x_4 of { (HappyWrap70 happy_var_4) -> 
	happyIn52
		 (AbsChapel.NoAssgmArrayDec happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_55 = happySpecReduce_3  48# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	case happyOut73 happy_x_3 of { (HappyWrap73 happy_var_3) -> 
	happyIn52
		 (AbsChapel.AssgmDec happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_56 = happyReduce 5# 48# happyReduction_56
happyReduction_56 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	case happyOut70 happy_x_3 of { (HappyWrap70 happy_var_3) -> 
	case happyOut20 happy_x_4 of { (HappyWrap20 happy_var_4) -> 
	case happyOut73 happy_x_5 of { (HappyWrap73 happy_var_5) -> 
	happyIn52
		 (AbsChapel.AssgmTypeDec happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}}}

happyReduce_57 = happySpecReduce_3  49# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut55 happy_x_2 of { (HappyWrap55 happy_var_2) -> 
	case happyOut9 happy_x_3 of { (HappyWrap9 happy_var_3) -> 
	happyIn53
		 (AbsChapel.ArrayDeclIndex happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_58 = happySpecReduce_3  49# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut9 happy_x_3 of { (HappyWrap9 happy_var_3) -> 
	happyIn53
		 (AbsChapel.ArrayDeclFixed happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_59 = happyReduce 4# 50# happyReduction_59
happyReduction_59 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut57 happy_x_1 of { (HappyWrap57 happy_var_1) -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	happyIn54
		 (AbsChapel.ArrayDim happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_60 = happySpecReduce_1  51# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	happyIn55
		 ((:[]) happy_var_1
	)}

happyReduce_61 = happySpecReduce_3  51# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_3 of { (HappyWrap55 happy_var_3) -> 
	happyIn55
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_62 = happySpecReduce_1  52# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut57 happy_x_1 of { (HappyWrap57 happy_var_1) -> 
	happyIn56
		 ((:[]) happy_var_1
	)}

happyReduce_63 = happySpecReduce_3  52# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { (HappyWrap57 happy_var_1) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn56
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_64 = happySpecReduce_1  53# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn57
		 (AbsChapel.ArrayBoundIdent happy_var_1
	)}

happyReduce_65 = happySpecReduce_1  53# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut85 happy_x_1 of { (HappyWrap85 happy_var_1) -> 
	happyIn57
		 (AbsChapel.ArratBoundConst happy_var_1
	)}

happyReduce_66 = happySpecReduce_1  54# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn58
		 ((:[]) happy_var_1
	)}

happyReduce_67 = happySpecReduce_3  54# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	case happyOut58 happy_x_3 of { (HappyWrap58 happy_var_3) -> 
	happyIn58
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_68 = happySpecReduce_1  55# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	happyIn59
		 (AbsChapel.PVarMode happy_var_1
	)}

happyReduce_69 = happySpecReduce_1  55# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	happyIn59
		 (AbsChapel.PConstMode happy_var_1
	)}

happyReduce_70 = happySpecReduce_3  56# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	case happyOut61 happy_x_2 of { (HappyWrap61 happy_var_2) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	happyIn60
		 (AbsChapel.FunDec happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_71 = happySpecReduce_2  57# happyReduction_71
happyReduction_71 happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	case happyOut62 happy_x_2 of { (HappyWrap62 happy_var_2) -> 
	happyIn61
		 (AbsChapel.SignNoRet happy_var_1 happy_var_2
	)}}

happyReduce_72 = happyReduce 4# 57# happyReduction_72
happyReduction_72 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	case happyOut62 happy_x_2 of { (HappyWrap62 happy_var_2) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	case happyOut70 happy_x_4 of { (HappyWrap70 happy_var_4) -> 
	happyIn61
		 (AbsChapel.SignWRet happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_73 = happySpecReduce_3  58# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut63 happy_x_2 of { (HappyWrap63 happy_var_2) -> 
	case happyOut7 happy_x_3 of { (HappyWrap7 happy_var_3) -> 
	happyIn62
		 (AbsChapel.FunParams happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_74 = happySpecReduce_0  59# happyReduction_74
happyReduction_74  =  happyIn63
		 ([]
	)

happyReduce_75 = happySpecReduce_1  59# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	happyIn63
		 ((:[]) happy_var_1
	)}

happyReduce_76 = happySpecReduce_3  59# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	case happyOut63 happy_x_3 of { (HappyWrap63 happy_var_3) -> 
	happyIn63
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_77 = happySpecReduce_3  60# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	case happyOut70 happy_x_3 of { (HappyWrap70 happy_var_3) -> 
	happyIn64
		 (AbsChapel.ParNoMode happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_78 = happyReduce 4# 60# happyReduction_78
happyReduction_78 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut72 happy_x_1 of { (HappyWrap72 happy_var_1) -> 
	case happyOut58 happy_x_2 of { (HappyWrap58 happy_var_2) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	case happyOut70 happy_x_4 of { (HappyWrap70 happy_var_4) -> 
	happyIn64
		 (AbsChapel.ParWMode happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_79 = happySpecReduce_3  61# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut66 happy_x_2 of { (HappyWrap66 happy_var_2) -> 
	case happyOut5 happy_x_3 of { (HappyWrap5 happy_var_3) -> 
	happyIn65
		 (AbsChapel.FunBlock happy_var_1 (reverse happy_var_2) happy_var_3
	)}}}

happyReduce_80 = happySpecReduce_0  62# happyReduction_80
happyReduction_80  =  happyIn66
		 ([]
	)

happyReduce_81 = happySpecReduce_2  62# happyReduction_81
happyReduction_81 happy_x_2
	happy_x_1
	 =  case happyOut66 happy_x_1 of { (HappyWrap66 happy_var_1) -> 
	case happyOut67 happy_x_2 of { (HappyWrap67 happy_var_2) -> 
	happyIn66
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_82 = happySpecReduce_1  63# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOut68 happy_x_1 of { (HappyWrap68 happy_var_1) -> 
	happyIn67
		 (AbsChapel.Stm happy_var_1
	)}

happyReduce_83 = happySpecReduce_2  63# happyReduction_83
happyReduction_83 happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { (HappyWrap60 happy_var_1) -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	happyIn67
		 (AbsChapel.Fun happy_var_1 happy_var_2
	)}}

happyReduce_84 = happySpecReduce_1  63# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn67
		 (AbsChapel.DeclStm happy_var_1
	)}

happyReduce_85 = happySpecReduce_1  63# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	happyIn67
		 (AbsChapel.Block happy_var_1
	)}

happyReduce_86 = happySpecReduce_3  63# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	case happyOut73 happy_x_2 of { (HappyWrap73 happy_var_2) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn67
		 (AbsChapel.RetVal happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_87 = happySpecReduce_2  63# happyReduction_87
happyReduction_87 happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	happyIn67
		 (AbsChapel.RetVoid happy_var_1 happy_var_2
	)}}

happyReduce_88 = happyReduce 4# 64# happyReduction_88
happyReduction_88 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	case happyOut69 happy_x_4 of { (HappyWrap69 happy_var_4) -> 
	happyIn68
		 (AbsChapel.DoWhile happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_89 = happySpecReduce_3  64# happyReduction_89
happyReduction_89 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	case happyOut69 happy_x_2 of { (HappyWrap69 happy_var_2) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	happyIn68
		 (AbsChapel.While happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_90 = happyReduce 4# 64# happyReduction_90
happyReduction_90 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	case happyOut69 happy_x_2 of { (HappyWrap69 happy_var_2) -> 
	case happyOut14 happy_x_3 of { (HappyWrap14 happy_var_3) -> 
	case happyOut65 happy_x_4 of { (HappyWrap65 happy_var_4) -> 
	happyIn68
		 (AbsChapel.If happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_91 = happyReduce 6# 64# happyReduction_91
happyReduction_91 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	case happyOut69 happy_x_2 of { (HappyWrap69 happy_var_2) -> 
	case happyOut14 happy_x_3 of { (HappyWrap14 happy_var_3) -> 
	case happyOut65 happy_x_4 of { (HappyWrap65 happy_var_4) -> 
	case happyOut15 happy_x_5 of { (HappyWrap15 happy_var_5) -> 
	case happyOut65 happy_x_6 of { (HappyWrap65 happy_var_6) -> 
	happyIn68
		 (AbsChapel.IfElse happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}}

happyReduce_92 = happySpecReduce_2  64# happyReduction_92
happyReduction_92 happy_x_2
	happy_x_1
	 =  case happyOut73 happy_x_1 of { (HappyWrap73 happy_var_1) -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	happyIn68
		 (AbsChapel.StExp happy_var_1 happy_var_2
	)}}

happyReduce_93 = happySpecReduce_3  65# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut73 happy_x_2 of { (HappyWrap73 happy_var_2) -> 
	case happyOut7 happy_x_3 of { (HappyWrap7 happy_var_3) -> 
	happyIn69
		 (AbsChapel.SGuard happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_94 = happySpecReduce_1  66# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn70
		 (AbsChapel.Tint happy_var_1
	)}

happyReduce_95 = happySpecReduce_1  66# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
	happyIn70
		 (AbsChapel.Treal happy_var_1
	)}

happyReduce_96 = happySpecReduce_1  67# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn71
		 (AbsChapel.AssgnEq happy_var_1
	)}

happyReduce_97 = happySpecReduce_1  67# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	happyIn71
		 (AbsChapel.AssgnPlEq happy_var_1
	)}

happyReduce_98 = happySpecReduce_1  68# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	happyIn72
		 (AbsChapel.RefMode happy_var_1
	)}

happyReduce_99 = happySpecReduce_3  69# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut73 happy_x_1 of { (HappyWrap73 happy_var_1) -> 
	case happyOut71 happy_x_2 of { (HappyWrap71 happy_var_2) -> 
	case happyOut74 happy_x_3 of { (HappyWrap74 happy_var_3) -> 
	happyIn73
		 (AbsChapel.EAss happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_100 = happySpecReduce_1  69# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	happyIn73
		 (happy_var_1
	)}

happyReduce_101 = happySpecReduce_3  70# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	case happyOut35 happy_x_2 of { (HappyWrap35 happy_var_2) -> 
	case happyOut75 happy_x_3 of { (HappyWrap75 happy_var_3) -> 
	happyIn74
		 (AbsChapel.Elor happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_102 = happySpecReduce_1  70# happyReduction_102
happyReduction_102 happy_x_1
	 =  case happyOut75 happy_x_1 of { (HappyWrap75 happy_var_1) -> 
	happyIn74
		 (happy_var_1
	)}

happyReduce_103 = happySpecReduce_3  71# happyReduction_103
happyReduction_103 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut75 happy_x_1 of { (HappyWrap75 happy_var_1) -> 
	case happyOut36 happy_x_2 of { (HappyWrap36 happy_var_2) -> 
	case happyOut76 happy_x_3 of { (HappyWrap76 happy_var_3) -> 
	happyIn75
		 (AbsChapel.Eland happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_104 = happySpecReduce_1  71# happyReduction_104
happyReduction_104 happy_x_1
	 =  case happyOut81 happy_x_1 of { (HappyWrap81 happy_var_1) -> 
	happyIn75
		 (happy_var_1
	)}

happyReduce_105 = happySpecReduce_3  72# happyReduction_105
happyReduction_105 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	case happyOut34 happy_x_2 of { (HappyWrap34 happy_var_2) -> 
	case happyOut77 happy_x_3 of { (HappyWrap77 happy_var_3) -> 
	happyIn76
		 (AbsChapel.Ebitand happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_106 = happySpecReduce_1  72# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOut77 happy_x_1 of { (HappyWrap77 happy_var_1) -> 
	happyIn76
		 (happy_var_1
	)}

happyReduce_107 = happySpecReduce_3  73# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut77 happy_x_1 of { (HappyWrap77 happy_var_1) -> 
	case happyOut37 happy_x_2 of { (HappyWrap37 happy_var_2) -> 
	case happyOut78 happy_x_3 of { (HappyWrap78 happy_var_3) -> 
	happyIn77
		 (AbsChapel.Eeq happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_108 = happySpecReduce_3  73# happyReduction_108
happyReduction_108 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut77 happy_x_1 of { (HappyWrap77 happy_var_1) -> 
	case happyOut38 happy_x_2 of { (HappyWrap38 happy_var_2) -> 
	case happyOut78 happy_x_3 of { (HappyWrap78 happy_var_3) -> 
	happyIn77
		 (AbsChapel.Eneq happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_109 = happySpecReduce_1  73# happyReduction_109
happyReduction_109 happy_x_1
	 =  case happyOut78 happy_x_1 of { (HappyWrap78 happy_var_1) -> 
	happyIn77
		 (happy_var_1
	)}

happyReduce_110 = happySpecReduce_3  74# happyReduction_110
happyReduction_110 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut78 happy_x_1 of { (HappyWrap78 happy_var_1) -> 
	case happyOut27 happy_x_2 of { (HappyWrap27 happy_var_2) -> 
	case happyOut83 happy_x_3 of { (HappyWrap83 happy_var_3) -> 
	happyIn78
		 (AbsChapel.Elthen happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_111 = happySpecReduce_3  74# happyReduction_111
happyReduction_111 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut78 happy_x_1 of { (HappyWrap78 happy_var_1) -> 
	case happyOut28 happy_x_2 of { (HappyWrap28 happy_var_2) -> 
	case happyOut83 happy_x_3 of { (HappyWrap83 happy_var_3) -> 
	happyIn78
		 (AbsChapel.Egrthen happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_112 = happySpecReduce_3  74# happyReduction_112
happyReduction_112 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut78 happy_x_1 of { (HappyWrap78 happy_var_1) -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	case happyOut83 happy_x_3 of { (HappyWrap83 happy_var_3) -> 
	happyIn78
		 (AbsChapel.Ele happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_113 = happySpecReduce_3  74# happyReduction_113
happyReduction_113 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut78 happy_x_1 of { (HappyWrap78 happy_var_1) -> 
	case happyOut40 happy_x_2 of { (HappyWrap40 happy_var_2) -> 
	case happyOut79 happy_x_3 of { (HappyWrap79 happy_var_3) -> 
	happyIn78
		 (AbsChapel.Ege happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_114 = happySpecReduce_1  74# happyReduction_114
happyReduction_114 happy_x_1
	 =  case happyOut83 happy_x_1 of { (HappyWrap83 happy_var_1) -> 
	happyIn78
		 (happy_var_1
	)}

happyReduce_115 = happySpecReduce_3  75# happyReduction_115
happyReduction_115 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut79 happy_x_1 of { (HappyWrap79 happy_var_1) -> 
	case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	case happyOut80 happy_x_3 of { (HappyWrap80 happy_var_3) -> 
	happyIn79
		 (AbsChapel.Eplus happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_116 = happySpecReduce_3  75# happyReduction_116
happyReduction_116 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut79 happy_x_1 of { (HappyWrap79 happy_var_1) -> 
	case happyOut30 happy_x_2 of { (HappyWrap30 happy_var_2) -> 
	case happyOut80 happy_x_3 of { (HappyWrap80 happy_var_3) -> 
	happyIn79
		 (AbsChapel.Eminus happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_117 = happySpecReduce_1  75# happyReduction_117
happyReduction_117 happy_x_1
	 =  case happyOut80 happy_x_1 of { (HappyWrap80 happy_var_1) -> 
	happyIn79
		 (happy_var_1
	)}

happyReduce_118 = happySpecReduce_3  76# happyReduction_118
happyReduction_118 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut80 happy_x_1 of { (HappyWrap80 happy_var_1) -> 
	case happyOut31 happy_x_2 of { (HappyWrap31 happy_var_2) -> 
	case happyOut84 happy_x_3 of { (HappyWrap84 happy_var_3) -> 
	happyIn80
		 (AbsChapel.Etimes happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_119 = happySpecReduce_3  76# happyReduction_119
happyReduction_119 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut80 happy_x_1 of { (HappyWrap80 happy_var_1) -> 
	case happyOut32 happy_x_2 of { (HappyWrap32 happy_var_2) -> 
	case happyOut84 happy_x_3 of { (HappyWrap84 happy_var_3) -> 
	happyIn80
		 (AbsChapel.Ediv happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_120 = happySpecReduce_3  76# happyReduction_120
happyReduction_120 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut80 happy_x_1 of { (HappyWrap80 happy_var_1) -> 
	case happyOut33 happy_x_2 of { (HappyWrap33 happy_var_2) -> 
	case happyOut84 happy_x_3 of { (HappyWrap84 happy_var_3) -> 
	happyIn80
		 (AbsChapel.Emod happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_121 = happySpecReduce_1  76# happyReduction_121
happyReduction_121 happy_x_1
	 =  case happyOut84 happy_x_1 of { (HappyWrap84 happy_var_1) -> 
	happyIn80
		 (happy_var_1
	)}

happyReduce_122 = happySpecReduce_1  77# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut82 happy_x_1 of { (HappyWrap82 happy_var_1) -> 
	happyIn81
		 (happy_var_1
	)}

happyReduce_123 = happySpecReduce_1  78# happyReduction_123
happyReduction_123 happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	happyIn82
		 (happy_var_1
	)}

happyReduce_124 = happySpecReduce_1  79# happyReduction_124
happyReduction_124 happy_x_1
	 =  case happyOut79 happy_x_1 of { (HappyWrap79 happy_var_1) -> 
	happyIn83
		 (happy_var_1
	)}

happyReduce_125 = happySpecReduce_3  80# happyReduction_125
happyReduction_125 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut73 happy_x_2 of { (HappyWrap73 happy_var_2) -> 
	case happyOut7 happy_x_3 of { (HappyWrap7 happy_var_3) -> 
	happyIn84
		 (AbsChapel.InnerExp happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_126 = happySpecReduce_1  80# happyReduction_126
happyReduction_126 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn84
		 (AbsChapel.Evar happy_var_1
	)}

happyReduce_127 = happySpecReduce_1  80# happyReduction_127
happyReduction_127 happy_x_1
	 =  case happyOut85 happy_x_1 of { (HappyWrap85 happy_var_1) -> 
	happyIn84
		 (AbsChapel.Econst happy_var_1
	)}

happyReduce_128 = happySpecReduce_1  80# happyReduction_128
happyReduction_128 happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	happyIn84
		 (AbsChapel.Estring happy_var_1
	)}

happyReduce_129 = happySpecReduce_1  81# happyReduction_129
happyReduction_129 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn85
		 (AbsChapel.Efloat happy_var_1
	)}

happyReduce_130 = happySpecReduce_1  81# happyReduction_130
happyReduction_130 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn85
		 (AbsChapel.Echar happy_var_1
	)}

happyReduce_131 = happySpecReduce_1  81# happyReduction_131
happyReduction_131 happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	happyIn85
		 (AbsChapel.Eint happy_var_1
	)}

happyNewToken action sts stk [] =
	happyDoAction 44# notHappyAtAll action sts stk []

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
	PT _ (T_PInt _) -> cont 16#;
	PT _ (T_PReal _) -> cont 17#;
	PT _ (T_PAssignmEq _) -> cont 18#;
	PT _ (T_PAssignmPlus _) -> cont 19#;
	PT _ (T_PRef _) -> cont 20#;
	PT _ (T_PVar _) -> cont 21#;
	PT _ (T_PConst _) -> cont 22#;
	PT _ (T_PProc _) -> cont 23#;
	PT _ (T_PReturn _) -> cont 24#;
	PT _ (T_PElthen _) -> cont 25#;
	PT _ (T_PEgrthen _) -> cont 26#;
	PT _ (T_PEplus _) -> cont 27#;
	PT _ (T_PEminus _) -> cont 28#;
	PT _ (T_PEtimes _) -> cont 29#;
	PT _ (T_PEdiv _) -> cont 30#;
	PT _ (T_PEmod _) -> cont 31#;
	PT _ (T_PDef _) -> cont 32#;
	PT _ (T_PElor _) -> cont 33#;
	PT _ (T_PEland _) -> cont 34#;
	PT _ (T_PEeq _) -> cont 35#;
	PT _ (T_PEneq _) -> cont 36#;
	PT _ (T_PEle _) -> cont 37#;
	PT _ (T_PEge _) -> cont 38#;
	PT _ (T_PIdent _) -> cont 39#;
	PT _ (T_PString _) -> cont 40#;
	PT _ (T_PChar _) -> cont 41#;
	PT _ (T_PDouble _) -> cont 42#;
	PT _ (T_PInteger _) -> cont 43#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 44# tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap46 x') = happyOut46 x} in x'))

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
