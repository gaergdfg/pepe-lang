# Makefile for building the parser and test program.

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean

# Default goal.

all :
	ghc --make Main.hs -o interpreter

# Rules for building the parser.

# Pepe/Abs.hs Pepe/Lex.x Pepe/Par.y Pepe/Print.hs Pepe/Test.hs : lang/Pepe.cf
# 	bnfc --haskell -d --functor lang/Pepe.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

Pepe/Test : Pepe/Abs.hs Pepe/Lex.hs Pepe/Par.hs Pepe/Print.hs Pepe/Test.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f Pepe/*.hi Pepe/*.o *.hi *.o

distclean : clean
	-rm -f Pepe/Abs.hs Pepe/Abs.hs.bak Pepe/ComposOp.hs Pepe/ComposOp.hs.bak Pepe/Doc.txt Pepe/Doc.txt.bak Pepe/ErrM.hs Pepe/ErrM.hs.bak Pepe/Layout.hs Pepe/Layout.hs.bak Pepe/Lex.x Pepe/Lex.x.bak Pepe/Par.y Pepe/Par.y.bak Pepe/Print.hs Pepe/Print.hs.bak Pepe/Skel.hs Pepe/Skel.hs.bak Pepe/Test.hs Pepe/Test.hs.bak Pepe/XML.hs Pepe/XML.hs.bak Pepe/AST.agda Pepe/AST.agda.bak Pepe/Parser.agda Pepe/Parser.agda.bak Pepe/IOLib.agda Pepe/IOLib.agda.bak Pepe/Main.agda Pepe/Main.agda.bak Pepe/Pepe.dtd Pepe/Pepe.dtd.bak Pepe/Test Pepe/Lex.hs Pepe/Par.hs Pepe/Par.info Pepe/ParData.hs Makefile
	-rmdir -p Pepe/
