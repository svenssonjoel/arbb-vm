

INCLUDES= -i../../../../ -i../../../ -i. -i../../../accelerate-examples/src/common -I../../../include \
          -i../../../dist/build/autogen

# INCLUDES= -i../../../../ 
LIBS= -ltbb -larbb_dev
#LIBS= -ltbb -larbb

all:
	ghc --make $(INCLUDES) Main.hs $(LIBS) -O2

clean:
	rm -f Main

