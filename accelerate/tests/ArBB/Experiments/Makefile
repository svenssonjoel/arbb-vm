



# INCLUDES= -i.. -itests/ArBB/Benchmarks -iaccelerate-examples/src/common -Iinclude

INCLUDES= -i../../../../ -i../../../ -i. -i../../../accelerate-examples/src/common -I../../../include
LIBS= -ltbb -larbb_dev
#LIBS= -ltbb -larbb

all:
	ghc --make $(INCLUDES) Main.hs $(LIBS) -O3 -rtsopts
#	ghc --make $(INCLUDES) tests/ArBB/Benchmarks/Main.hs $(LIBS)

c:
#	gcc -fast dotp.cpp
	icc -fast dotp.cpp

clean:
	rm -f Main

# $ make
# [CC] bs_c.cpp
# g++ -m64 -DSAMPLE_DATA_SET -D__POSIX__ -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -Wall -MMD -msse2 -Wno-unknown-pragmas -Wno-unused-variable -I../../../include -I../../../samples/common -DNDEBUG -O2 -c -o opt_intel64/bs_c.o bs_c.cpp

# [CC] bs_arbb.cpp
# g++ -m64 -DSAMPLE_DATA_SET -D__POSIX__ -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -Wall -MMD -msse2 -Wno-unknown-pragmas -Wno-unused-variable -I../../../include -I../../../samples/common -DNDEBUG -O2 -c -o opt_intel64/bs_arbb.o bs_arbb.cpp

# [CC] main.cpp
# g++ -m64 -DSAMPLE_DATA_SET -D__POSIX__ -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -Wall -MMD -msse2 -Wno-unknown-pragmas -Wno-unused-variable -I../../../include -I../../../samples/common -DNDEBUG -O2 -c -o opt_intel64/main.o main.cpp

# [CC] data.cpp
# g++ -m64 -DSAMPLE_DATA_SET -D__POSIX__ -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -Wall -MMD -msse2 -Wno-unknown-pragmas -Wno-unused-variable -I../../../include -I../../../samples/common -DNDEBUG -O2 -c -o opt_intel64/data.o data.cpp

# [CC] util.cpp
# g++ -m64 -DSAMPLE_DATA_SET -D__POSIX__ -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -Wall -MMD -msse2 -Wno-unknown-pragmas -Wno-unused-variable -I../../../include -I../../../samples/common -DNDEBUG -O2 -c -o opt_intel64/util.o ../../../samples/common/util.cpp

# [LD] black-scholes
# g++ -m64  -o ./opt_intel64/black-scholes ./opt_intel64/bs_c.o ./opt_intel64/bs_arbb.o ./opt_intel64/main.o ./opt_intel64/data.o ./opt_intel64/util.o   -L../../../lib/intel64 -larbb -ltbb  -lpthread -lrt -ldl
