

# RRN: Note, cabal-based distribution is easie if GHC can build the .c
# rather than an external Makefile.  Might want to fix this if possible.

arbb_vmwrap.o: arbb_vmwrap.c arbb_vmwrap.h
	gcc -c arbb_vmwrap.c -o arbb_vmwrap.o 
# -I$(ARBBD)/include


clean:
	rm -f arbb_vmwrap.o
