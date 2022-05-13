all:
	ghc --make Pepe/Main.hs -o interpreter

clean:
	-rm -f Pepe/*.hi Pepe/*.o
