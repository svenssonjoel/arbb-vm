{-# Language FlexibleContexts, CPP, ScopedTypeVariables #-}


module Main where 


-- BENCHMARKS
import UseNode


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
  arr   <- randomUArrayR (0,100)    gen n
  input <- evaluate$ Acc.fromIArray arr
  t_g_2 <- getCurrentTime 
  putStrLn$ "Done generating input data: " ++ ( show (diffUTCTime t_g_2 t_g_1))
  

  -- The timing run 
  t_p_1 <- getCurrentTime
  r' <-  evaluate$ ArBB.run (useNodeAcc input)
  t_p_2 <- getCurrentTime
  r0' <- evaluate$ CUDA.run (useNodeAcc input) 
  t_p_3 <- getCurrentTime 



  putStrLn$ "UseNode: " ++ if checkResult (toList r') (toList r0') then "Passed" else "failed "
  putStrLn$ "Time ArBB : " ++ ( show (diffUTCTime t_p_2 t_p_1) )  
  putStrLn$ "Time CUDA : " ++ ( show (diffUTCTime t_p_3 t_p_2) )  

  return ()



checkResult [] [] = True
checkResult (a:as) (b:bs) = abs (a - b) < 0.001 &&
                            checkResult as bs 
checkResult _ _ = False

