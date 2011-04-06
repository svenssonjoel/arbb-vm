#!/bin/sh

echo RUNNING TEST WITH CUDA BACK-END WARMED UP
echo ------------------------------------------------------------------------------
for i in 100000 250000 500000 750000 1000000 2500000 5000000 7500000 10000000 20000000; do 
   echo Running test with $i elements
   ./Main $i y 
done
echo ------------------------------------------------------------------------------
echo 
echo 
echo RUNNING TEST WITH CUDA BACK-END ***NOT*** WARMED UP
echo ------------------------------------------------------------------------------
for i in 100000 250000 500000 750000 1000000 2500000 5000000 7500000 10000000 20000000; do 
   echo Running test with $i elements
   ./Main $i n 
done