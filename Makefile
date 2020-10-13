# Makefile generated by BNFC.

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : TestChapel ChapelParse

# Rules for building the parser.

ErrM.hs LexChapel.x PrintChapel.hs ParChapel.y TestChapel.hs : chapel.cf
	bnfc --haskell chapel.cf

%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

TestChapel : TestChapel.hs ErrM.hs LexChapel.hs ParChapel.hs PrintChapel.hs
	ghc --make $< -o $@

ChapelParse : ChapelParse.hs ErrM.hs LexChapel.hs ParChapel.hs PrintChapel.hs
	ghc --make $< -o $@
# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsChapel.hs AbsChapel.hs.bak ComposOp.hs ComposOp.hs.bak DocChapel.txt DocChapel.txt.bak ErrM.hs ErrM.hs.bak LayoutChapel.hs LayoutChapel.hs.bak LexChapel.x LexChapel.x.bak ParChapel.y ParChapel.y.bak PrintChapel.hs PrintChapel.hs.bak SharedString.hs SharedString.hs.bak SkelChapel.hs SkelChapel.hs.bak TestChapel.hs TestChapel.hs.bak XMLChapel.hs XMLChapel.hs.bak ASTChapel.agda ASTChapel.agda.bak ParserChapel.agda ParserChapel.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak chapel.dtd chapel.dtd.bak TestChapel LexChapel.hs ParChapel.hs ParChapel.info ParDataChapel.hs Makefile


# EOF
