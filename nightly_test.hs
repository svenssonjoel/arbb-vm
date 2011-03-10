#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables  #-}
-- MUST be run from the directory containing this script.

import System.Environment
import NightlyTester 

repo = "arbb-vm"

----------------------------------------------------------------------------------------------------
-- Main Script

main =  
   getArgs >>= \ emails ->
   runNightlyTest (mkDefaultConfig repo emails) $ 
 do 
  gitReportHead
  reportMachineInfo

  section "Clean and build everything"
  mrun "cabal clean"
  mrun "cabal configure"
  mrun "cabal build"

  section "Build all tests:"
  mrun "make"

  section "Run all tests:"
  mrun "make test"

  newline

