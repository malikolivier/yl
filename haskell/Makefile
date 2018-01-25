.PHONY: yl yl-debug

yl:
	ghc -O2 --make $@

yl-debug:
	ghc --make yl -prof -fprof-auto -fprof-cafs

clean:
	rm -rf *.o *.hi yl
