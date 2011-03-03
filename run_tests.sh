#!/bin/bash

# Fail on any failed command:
# set -e

# Just a litle helper for the Makefile.

# Need to add proper regression testing framework...

# CMDS=$@

# for CMD in $CMDS; do 
for CMD in "$@"; do 
  echo 
  echo "Running test: "$CMD
  echo "================================================================================"

  ./$CMD
  CODE=$?
  if [ "$CODE" != 0 ]; then
     echo "ERROR: command $CMD failed with code $CODE"
     exit $CODE
  fi
done
