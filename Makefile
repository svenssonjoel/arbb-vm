
# NOTE: This Makefile assumes that you have sources "install_environment_vars.sh"

TESTS= \
   examples/tests/Test_Simple1.hs \
   examples/tests/Test_Simple2.hs \
   examples/tests/Test_MultipleDefaultContextCalls.hs

TESTEXES = $(TESTS:.hs=.exe)


all: Intel/ArbbVM.hs tests

# Preprocessing step:
Intel/ArbbVM.hs: Intel/ArbbVM.chs 
	c2hs Intel/ArbbVM.chs 

# Using an external Makefile for the cbits for now...
cbits/arbb_vmwrap.o: 
	cd cbits; $(MAKE)

tests: $(TESTEXES)

%.exe: %.hs cbits/arbb_vmwrap.o
	ghc -o $@ --make $< cbits/arbb_vmwrap.o -L$(ARBBD)/lib/$(ARBB_ARCH) -ltbb -larbb -lpthread 

runtests: 
	echo $(TESTEXES) | xargs -n1 bash -c 
# Eek, having the usual shell scripting quotation problems:
#	@for exe in $(TESTEXES); do echo "\n========================================"; './$exe'; done

clean:
	rm -f *.o *.hi *.chi Intel/ArbbVM.hs Intel/*.hi Intel/*.o Intel/*.chi Intel/*.chs.h
	rm -f $(TESTEXES) $(TESTS:.hs=.hi) $(TESTS:.hs=.o)
	cd cbits; $(MAKE) clean
