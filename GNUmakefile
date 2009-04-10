.PHONY: all clean markup report test

TESTS = Test
ifeq (,$(shell ghc-pkg list dataenc | grep dataenc))
GHCOPTS = -fhpc -isrc
else
GHCOPTS = -fhpc -hide-package dataenc -isrc
endif
HPC_SUM_OPTS = --exclude=Main --exclude=DataencUT --exclude=DataencQC

% : %.hs
	ghc --make $(GHCOPTS) $<

all: $(TESTS)

test: $(TESTS)
	./Test

report : test
	hpc6 sum $(HPC_SUM_OPTS) --output test.tix Test.tix
	hpc6 report test.tix

markup : test
	hpc6 sum $(HPC_SUM_OPTS) --output test.tix Test.tix
	hpc6 markup test.tix

clean:
	rm -f *~ Test *.tix *.html *.o *.hi
	rm -rf .hpc
	rm -f src/Codec/Binary/*.o
	rm -f src/Codec/Binary/*.hi
	rm -f src/Codec/Binary/*~
