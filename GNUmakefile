.PHONY: all clean markup report test really-clean

TESTS = dist/build/tests/tests

HPC = hpc
HPC_SUM_OPTS = --exclude=Main --exclude=DataencUT --exclude=DataencQC

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
	rm -f src/Codec/Binary/*.o
	rm -f src/Codec/Binary/*.hi
	rm -f src/Codec/Binary/*~

really-clean: clean
	rm -rf .hpc
	./Setup.hs clean
