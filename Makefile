
# NOTE: This Makefile assumes that you have sources "install_environment_vars.sh"

all: ArbbVM.hs tests

# ARRBD=/opt/intel/arbb/latest/
# ARRB_ARCH=ia32
# ARRB_ARCH=intel64

ArbbVM.hs: ArbbVM.chs 
	c2hs ArbbVM.chs 


arbb_vmwrap.o: arbb_vmwrap.c 
	gcc -c arbb_vmwrap.c -o arbb_vmwrap.o 
# -I$(ARBBD)/include

TESTS= \
   Test.hs Test2.hs \
   examples/tests/Test_MultipleDefaultContextCalls.hs

TESTEXES = $(TESTS:.hs=.exe)

tests: $(TESTEXES)

%.exe: %.hs arbb_vmwrap.o
	ghc -o $@ --make $< arbb_vmwrap.o -L$(ARBBD)/lib/$(ARBB_ARCH) -ltbb -larbb -lpthread 

clean:
	rm -f *.o *.hi *.chi ArbbVM.hs $(TESTEXES)
