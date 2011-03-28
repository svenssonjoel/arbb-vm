{-# Language FlexibleContexts #-}


-- Compile instructions: (should GHC optimizations be used ?) 
-- ghc --make -i.. -itests/ArBB/Benchmarks -iaccelerate-examples/src/common -Iinclude tests/ArBB/Benchmarks/Main.hs -ltbb -larbb_dev

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
import Control.Exception
import Data.Time

import System.Random.MWC 
import Random -- accelerate-examples/src/common/Random.hs



main = withSystemRandom $ \gen -> do
  putStrLn "Generating input data..." 
  v1    <- randomUArrayR (-1,1) gen 1000000
  v2    <- randomUArrayR (-1,1) gen 1000000
  v1'   <- convertUArray v1
  v2'   <- convertUArray v2
  alpha <- uniform gen
  putStrLn "Done generating input data!"
    
  -- TODO: How can I time just the exection of these ! (toList not included)  
  
  t_s_1 <- getCurrentTime
  r1' <- evaluate$ ArBB.run (saxpyAcc alpha  v1' v2')
  t_s_2 <- getCurrentTime 
  r2' <- evaluate$ Interp.run (saxpyAcc alpha v1' v2') 
  t_s_3  <- getCurrentTime
 
  
 
  let r1 = Sugar.toList r1'
      r2 = Sugar.toList r2'   

  putStrLn$ "Saxpy: " ++ if checkResult r1 r2 == [] then "Passed" else "failed" 
  putStrLn$ "Time ArBB : " ++ ( show (diffUTCTime t_s_2 t_s_1) )   
  putStrLn$ "Time InterP : " ++ ( show (diffUTCTime t_s_3 t_s_2) )  
  -- putStrLn$ show $ take 10 $ checkResult r1 r2

  t_dp_1 <- getCurrentTime                          
  r4' <- evaluate$  ArBB.run (dotpAcc v1' v2') 
  t_dp_2 <- getCurrentTime
  r5' <- evaluate$  Interp.run (dotpAcc v1' v2')   
  t_dp_3 <- getCurrentTime

  let r4 = Sugar.toList r4'
      r5 = Sugar.toList r5'
  
  putStrLn$ "DotProduct: " ++ if checkResult r4 r5 == [] then "Passed" else ("failed " ++ show (head (checkResult r4 r5)) )
  putStrLn$ "Time ArBB : " ++ ( show (diffUTCTime t_dp_2 t_dp_1) )  
  putStrLn$ "Time InterP : " ++ ( show (diffUTCTime t_dp_3 t_dp_2) )  
  
  return ()


checkResult [] [] = [] 
checkResult (x:xs) (y:ys) | 0.001 < abs (x - y) = (x,y) : checkResult xs ys 
                          | otherwise = checkResult xs ys 

