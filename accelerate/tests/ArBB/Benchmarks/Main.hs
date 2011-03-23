{-# Language FlexibleContexts #-}


-- Compile instructions: (should GHC optimizations be used ?) 
-- ghc --make -i.. -itests/ArBB/Benchmarks -Iinclude tests/ArBB/Benchmarks/Main.hs -ltbb -larbb_dev

-- Run instructions: (also try other opt levels) 
-- ARBB_OPT_LEVEL=o2 tests/ArBB/Benchmarks/Main

module Main where 


-- BENCHMARKS 
import Saxpy 
import DotP
import Matrix 


--- Accelerate stuff 
import Data.Array.IArray 
import Data.Array.Unboxed
import qualified Data.Array.Accelerate as Acc 
import qualified Data.Array.Accelerate.Smart as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar

-- Interpreter back-end
import qualified Data.Array.Accelerate.Interpreter as Interp

-- ArBB back-end 
import qualified Data.Array.Accelerate.ArBB as ArBB

-- CUDA back-end 
-- import qualified Data.Array.Accelerate.CUDA  

import Data.Int

import System.Random.MWC 
import Random -- accelerate-examples/src/common/Random.hs
import Validate --


main = withSystemRandom $ \gen -> do
  v1    <- randomUArrayR (-1,1) gen 1024
  v2    <- randomUArrayR (-1,1) gen 1024
  v1'   <- convertUArray v1
  v2'   <- convertUArray v2
  alpha <- uniform gen
    
  -- TODO: How can I time just the exection of these ! (toList not included)  
  let r1 = Sugar.toList$ ArBB.run (saxpyAcc alpha  v1' v2')
      r2 = Sugar.toList$ Interp.run (saxpyAcc alpha v1' v2') 
      -- r3 = Sugar.toList$ CUDA.run (saxpyAcc alpha v1' v2') 
  

  putStrLn$ "Saxpy: " ++ if checkResult r1 r2 == [] then "Passed" else "failed" 
  -- putStrLn$ show $ take 10 $ checkResult r1 r2

  let r4 = Sugar.toList$ ArBB.run (dotpAcc v1' v2') 
      r5 = Sugar.toList$ Interp.run (dotpAcc v1' v2')   
  
  putStrLn$ "DotProduct: " ++ if checkResult r4 r5 == [] then "Passed" else "failed" 
  return ()


checkResult [] [] = [] 
checkResult (x:xs) (y:ys) | 0.00001 < abs (x - y) = (x,y) : checkResult xs ys 
                          | otherwise = checkResult xs ys 

notTooDifferent f1 f2 = 0.1 < abs (f1 - f2) 