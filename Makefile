

all: ArbbVM.hs Test

ArbbVM.hs: ArbbVM.chs 
	c2hs ArbbVM.chs 


arbb_vmwrap.o: arbb_vmwrap.c 
	gcc -c arbb_vmwrap.c -o arbb_vmwrap.o

Test: Test.hs arbb_vmwrap.o
	ghc --make Test.hs arbb_vmwrap.o -L/opt/intel/arbb/1.0.0.015/lib/ia32 -ltbb -larbb -lpthread