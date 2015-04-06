GHC = ghc 

normal:
	$(GHC) Main.hs 

instrument:
	$(GHC) -fhpc Main.hs

clean:
	-rm *.hi
	-rm *.o
	-rm *.tix
	-rm *.exe
	-rm coverage/*
	-rm -rf ./.hpc

