{-# Language FlexibleContexts, CPP #-}


module Main where 


-- BENCHMARKS
import BlackScholes 


--- Accelerate stuff 
import Data.Array.IArray 
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


n = 100000

main = withSystemRandom $ \gen -> do
  putStrLn "Generating input data..." 
  t_g_1 <- getCurrentTime
  v_sp <- randomUArrayR (3,30)     gen n
  v_os <- randomUArrayR (1,100)    gen n
  v_oy <- randomUArrayR (0.25, 10) gen n 
  t_g_2 <- getCurrentTime
  a_psy <- evaluate$  Acc.fromList (Sugar.listToShape [n]) $ zip3 (elems v_sp) (elems v_os) (elems v_oy)
  t_g_3 <- getCurrentTime
  
  putStrLn$ "Done generating input data: " ++ ( show (diffUTCTime t_g_2 t_g_1) )  
  putStrLn$ "Zip stage took: " ++ show (diffUTCTime t_g_3 t_g_2) 

  t_p_1 <- getCurrentTime
  r' <-  evaluate$ ArBB.run (blackscholesAcc a_psy)
  t_p_2 <- getCurrentTime
  r0' <- evaluate$ CUDA.run (blackscholesAcc a_psy) 
  t_p_3 <- getCurrentTime 

--  putStrLn$ "BlackScholes: " ++ if checkResult r r0 == [] then "Passed" else "failed "
  putStrLn$ "Time ArBB : " ++ ( show (diffUTCTime t_p_2 t_p_1) )  
  putStrLn$ "Time InterP : " ++ ( show (diffUTCTime t_p_3 t_p_2) )  

  putStrLn$ show$ take 5$ Prelude.zip (toList r') (toList r0')
--   putStrLn$ show$ toList r0'
  
  return ()
