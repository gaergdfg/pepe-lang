all:
	happy -gca Pepe/Par.y
	alex -g Pepe/Lex.x
	ghc --make Pepe/Main.hs -o interpreter

clean:
	-rm -f Pepe/*.log Pepe/*.aux Pepe/*.hi Pepe/*.o Pepe/*.dvi

distclean: clean
	-rm -f Pepe/Doc.* Pepe/Lex.* Pepe/Par.* Pepe/Layout.* Pepe/Skel.* Pepe/Print.* Pepe/Test.* Pepe/Abs.* Pepe/Test Pepe/ErrM.* Pepe/SharedString.* Pepe/ComposOp.* Pepe/Pepe.dtd Pepe/XML.* Makefile*
		-rmdir -p Pepe/
