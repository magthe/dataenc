.PHONY: all clean markup report test

TESTS = UT QC
GHCOPTS = -fhpc -hide-package dataenc -isrc
# us this if dataenc isn't installed
#GHCOPTS = -fhpc -isrc

% : %.hs
	ghc --make $(GHCOPTS) $<

all: $(TESTS)

test: $(TESTS)
	./UT
	./QC

report : test
	hpc6 sum --exclude=Main --output test.tix QC.tix UT.tix
	hpc6 report test.tix

markup : test
	hpc6 sum --exclude=Main --output test.tix QC.tix UT.tix
	hpc6 markup test.tix

clean:
	rm -f *~ UT QC *.tix *.html
	rm -rf .hpc
	rm -f $(patsubst %,%.o,$(TESTS)) $(patsubst %,%.hi,$(TESTS))
	rm -f src/Codec/Binary/{*.o,*.hi,*~}
