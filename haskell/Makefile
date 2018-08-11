GHC = ghc -funbox-strict-fields -O2

test: main.hs cata3.hs orders.hs memotrie.hs dice.hs hmm.hs
	   ${GHC} --make -o $@ $<

clean :
	   rm -f *.o *.hi
