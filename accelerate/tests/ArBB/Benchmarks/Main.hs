{-# Language FlexibleContexts #-}
-- ghc --make -i.. -itests/ArBB/Benchmarks -Iinclude tests/ArBB/Benchmarks/Main.hs -ltbb -larbb_dev

module Main where 

import Saxpy 



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
  v1    <- randomUArrayR (-1,1) gen 100000
  v2    <- randomUArrayR (-1,1) gen 100000
  v1'   <- convertUArray v1
  v2'   <- convertUArray v2
  alpha <- return 1.0 -- uniform gen
    
  -- TODO: How can I time just the exection of these !   
  let r1 = Sugar.toList$ ArBB.run (saxpyAcc alpha  v1' v2')
      r2 = Sugar.toList$ Interp.run (saxpyAcc alpha v1' v2') 
      -- r3 = Sugar.toList$ CUDA.run (saxpyAcc alpha v1' v2') 
  
  --putStrLn$ show $ and $ zipWith  notTooDifferent r1 r2
 
  -- The results are "very" different. I don't really know 
  -- how to evaluate that.. 
  putStrLn$ show $ take 10 $ checkResult r1 r2
  return ()


checkResult [] [] = [] 
checkResult (x:xs) (y:ys) | 0.1 < abs (x - y) = (x,y) : checkResult xs ys 
                          | otherwise = checkResult xs ys 

notTooDifferent f1 f2 = 0.01 < abs (f1 - f2) 