#!/bin/bash

# TEMPORARY SCRIPT

set -e

make lib
rm -f snapshot*.bin

ghc --make test_trivial.hs -ltbb -larbb -o test_trivial.exe 
./test_trivial.exe 

echo 
echo FILE CONTENTS:
cat reproducer.c 

#CC=icc
CC=g++
# CC=clang

$CC reproducer.c -ltbb -larbb -I/opt/intel/arbb/latest/include/
echo 
echo RUNNING
./a.out
