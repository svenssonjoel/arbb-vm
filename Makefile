
# NOTE: This Makefile assumes that you have sources "install_environment_vars.sh"

TESTS= \
   examples/tests/Test_Simple2.hs \
   examples/tests/Test_Simple1.hs \
   examples/tests/Test_Reduce1.hs \
   examples/tests/Test_Reduce2.hs \
   examples/tests/Test_Reduce3.hs \
   examples/tests/Test_DotProd.hs \
   examples/tests/Test_Conditional1.hs \
   examples/tests/Test_Loop1.hs \
   examples/tests/Test_Constants.hs \
   examples/tests/Test_Map1.hs \
   examples/tests/Test_MultipleDefaultContextCalls.hs \
   examples/tests/Test_FunctionCalls.hs \
   examples/tests/Test_FunctionCalls2.hs \
   examples/tests/Test_FunctionCalls3.hs

SRC = Intel/ArbbVM.hs Intel/ArbbVM/Convenience.hs

TESTEXES = $(TESTS:.hs=.exe)

# Set this to arbb_dev (instead of 'arbb') to respect ARBB_OPT_LEVEL
# ARBB_LIB= arbb_dev
ARBB_LIB= arbb

all: lib tests

lib: Intel/ArbbVM.o

Intel/ArbbVM.o: Intel/ArbbVM.hs
	ghc --make $<

# Preprocessing step:
Intel/ArbbVM.hs: Intel/ArbbVM.chs 
	c2hs Intel/ArbbVM.chs 

# Using an external Makefile for the cbits for now...
# cbits/arbb_vmwrap.o: 
# 	cd cbits; $(MAKE)

tests: $(TESTEXES)

# cbits/arbb_vmwrap.o cbits/arbb_vmwrap.o
# %.exe: %.hs $(SRC)
%.exe: %.hs 
	ghc -o $@ --make $<  -L$(ARBBD)/lib/$(ARBB_ARCH) -ltbb -l$(ARBB_LIB) -lpthread 

temp:
	ghc -o examples/vm_haskell/Mandel.exe --make examples/vm_haskell/Mandel.hs -L$(ARBBD)/lib/$(ARBB_ARCH) -ltbb -l$(ARBB_LIB) -lpthread 

runtests: 
	echo $(TESTEXES) | xargs -n1 ./run_one_test.sh
#	echo $(TESTEXES) | xargs -n1 bash -c 
# Eek, having the usual shell scripting quotation problems:
#	@for exe in $(TESTEXES); do echo "\n========================================"; './$exe'; done

clean:
	rm -f *.o *.hi *.chi Intel/ArbbVM.hs Intel/*.hi Intel/*.o Intel/*.chi Intel/*.chs.h
	rm -f $(TESTEXES) $(TESTS:.hs=.hi) $(TESTS:.hs=.o)

#cd cbits; $(MAKE) clean
