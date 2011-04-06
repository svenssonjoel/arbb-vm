#!/bin/sh

echo #### RUNNING TEST CUDA BACK-END WARMED UP ####

./Main 100000 y 
./Main 250000 y 
./Main 500000 y 
./Main 750000 y
./Main 1000000 y
./Main 2500000 y
./Main 5000000 y
./Main 7500000 y 
./Main 10000000 y
./Main 20000000 y 

echo #### RUNNING TEST WITHOUT WARMING UP CUDA ####

./Main 100000 n 
./Main 250000 n 
./Main 500000 n 
./Main 750000 n
./Main 1000000 n
./Main 2500000 n
./Main 5000000 n
./Main 7500000 n 
./Main 10000000 n
./Main 20000000 n 

