.PHONY: all clean markup report test

TESTS = dist/build/tests/tests

HPC = hpc
#HPC_SUM_OPTS = --exclude=Main --exclude=DataencUT --exclude=DataencQC
HPC_SUM_OPTS = 

all: $(TESTS)

test: $(TESTS)
	for t in $(TESTS); do ./$${t}; done

report : test
	$(HPC) sum $(HPC_SUM_OPTS) --output run_test.tix tests.tix
	$(HPC) report run_test.tix

markup : test
	$(HPC) sum $(HPC_SUM_OPTS) --output run_test.tix tests.tix
	$(HPC) markup run_test.tix

clean:
	rm -f *~ *.tix *.html *.o *.hi
	#rm -rf .hpc
	rm -f src/Codec/Binary/*.o
	rm -f src/Codec/Binary/*.hi
	rm -f src/Codec/Binary/*~
