

all: ArbbVM.hs Test

# ARRBD=/opt/intel/arbb/latest/
# ARRB_ARCH=ia32
# ARRB_ARCH=intel64

ArbbVM.hs: ArbbVM.chs 
	c2hs ArbbVM.chs 


arbb_vmwrap.o: arbb_vmwrap.c 
	gcc -c arbb_vmwrap.c -o arbb_vmwrap.o 
# -I$(ARBBD)/include

Test: Test.hs arbb_vmwrap.o
	ghc --make Test.hs arbb_vmwrap.o -L$(ARBBD)/lib/$(ARBB_ARCH) -ltbb -larbb -lpthread 


clean:
	rm -f *.o *.hi *.chi ArbbVM.hs Test



