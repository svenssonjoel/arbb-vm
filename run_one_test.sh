#!/bin/bash

# Just a litle helper for the Makefile.

# Need to add proper regression testing framework...

CMD=$1

echo 
echo "Running test: "$CMD
echo "================================================================================"

exec $CMD
