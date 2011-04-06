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
-- import qualified Data.Array.Accelerate.CUDA as CUDA

import Data.Int
import Control.Exception
import Control.Monad
import Data.Time

import System.Random.MWC 
import Random -- accelerate-examples/src/common/Random.hs


n = 2^20

main = withSystemRandom $ \gen -> do
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

  t_p_1 <- getCurrentTime
  r' <-  evaluate$ ArBB.run (blackscholesAcc a_psy)
  t_p_2 <- getCurrentTime
--   r0' <- evaluate$ CUDA.run (blackscholesAcc a_psy) 
  t_p_3 <- getCurrentTime 

--  putStrLn$ "BlackScholes: " ++ if checkResult r r0 == [] then "Passed" else "failed "
  putStrLn$ "Time ArBB : " ++ ( show (diffUTCTime t_p_2 t_p_1) )  
  putStrLn$ "Time CUDA : " ++ ( show (diffUTCTime t_p_3 t_p_2) )  

--   putStrLn$ show$ take 1$ Prelude.zip (toList r') (toList r0')
--   putStrLn$ show$ toList r0'
  
  return ()
