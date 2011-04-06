{-# Language FlexibleContexts, CPP, ScopedTypeVariables #-}


module Main where 


-- BENCHMARKS
import BlackScholes 


--- Accelerate stuff 
import Data.Array.IArray as IArray
import Data.Array.Unboxed (UArray)
import Data.Array.Accelerate as Acc 
import qualified Data.Array.Accelerate.Smart as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar

-- Interpreter back-end
import qualified Data.Array.Accelerate.Interpreter as Interp

-- ArBB back-end 
import qualified Data.Array.Accelerate.ArBB as ArBB


-- CUDA back-end 
import qualified Data.Array.Accelerate.CUDA as CUDA

import Data.Int
import Control.Exception
import Control.Monad
import Data.Time

import System.Random.MWC 
import Random -- accelerate-examples/src/common/Random.hs

import System.IO
import System.Exit
import System.Directory
import System.Environment

--import Control.DeepSeq



main :: IO ()
main = do
  args <- getArgs 
  case args of 
     [n] -> run (read n) True 
     [n,w] -> run (read n) (w == "y") 
     _ -> error "wrong arg"
    
  
run n w = withSystemRandom $ \gen -> do
  putStrLn "Generating input data..." 
  t_g_1 <- getCurrentTime
  v_sp <- randomUArrayR (3,30)     gen n
  v_os <- randomUArrayR (1,100)    gen n
  v_oy <- randomUArrayR (0.25, 10) gen n 
  t_g_2 <- getCurrentTime
  v_psy :: IArray.Array Int (Float,Float,Float) <- evaluate$ listArray (0,n-1) $ zip3 (elems v_sp) (elems v_os) (elems v_oy)
  a_psy <- evaluate$ Acc.fromIArray v_psy
  t_g_3 <- getCurrentTime 
  putStrLn$ "Done generating input data: " ++ ( show (diffUTCTime t_g_2 t_g_1) )  
  putStrLn$ "Zip stage took: " ++ show (diffUTCTime t_g_3 t_g_2) 

  b <- evaluate$ toList a_psy
  putStrLn$ show (last b) 

  if w 
   then do 
    --n_sp <- randomUArrayR (3,30)     gen 2048
    --n_os <- randomUArrayR (1,100)    gen 2048
    --n_oy <- randomUArrayR (0.25, 10) gen 2048
    --n_g_2 <- getCurrentTime
    --n_psy :: IArray.Array Int (Float,Float,Float) <- evaluate$ listArray (0,2047) $ zip3 (elems n_sp) (elems n_os) (elems n_oy)
    --na_psy <- evaluate$ Acc.fromIArray n_psy

    --r <-  evaluate$ Interp.run (blackscholesAcc a_psy)
    putStrLn$ "sent array through interpreter: "
    --r <- evaluate$ CUDA.run (blackscholesAcc a_psy) 
    --putStrLn$ "warmed up CUDA: " ++ show (head (toList r)) 
   else return () 

  putStrLn$ show (head (toList a_psy))   

  -- The timing run 
  t0 <- getCurrentTime
  r' <-  evaluate$ ArBB.run (blackscholesAcc a_psy)   
  t1 <- getCurrentTime

  t2 <- getCurrentTime
  r0' <- evaluate$ CUDA.run (blackscholesAcc a_psy)        
  t3 <- getCurrentTime 

--  putStrLn$ "BlackScholes: " ++ if checkResult (toList r') (toList r0') then "Passed" else "failed "
  putStrLn$ "Time ArBB : " ++ ( show (diffUTCTime t3 t2) )  
  putStrLn$ "Time CUDA : " ++ ( show (diffUTCTime t1 t0) )  

  return ()



checkResult [] [] = True
checkResult ((a,a'):as) ((b,b'):bs) = abs (a - b) < 0.001 &&
                                      abs (a' - b') < 0.001 && 
                                      checkResult as bs 
checkResult _ _ = False

