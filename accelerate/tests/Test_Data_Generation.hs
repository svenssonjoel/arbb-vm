
{-# Language FlexibleContexts, CPP, BangPatterns, ScopedTypeVariables  #-}

-- This program tests vector/array creation.
-- We have had severe performance problems with certain methods of vector creation.
--
--   -RRN [2011.04.03]

module Main where 

-- BENCHMARKS
-- import BlackScholes 

--- Accelerate stuff 
import Data.Array as A
import Data.Array.IArray as IA
import Data.Array.Unboxed (UArray)
import Data.Array.Accelerate as Acc 
import qualified Data.Array.Accelerate.Interpreter as Interp

import qualified Data.Array.Accelerate.Smart as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar

-- Interpreter back-end
-- import qualified Data.Array.Accelerate.Interpreter as Interp

-- ArBB back-end 
-- import qualified Data.Array.Accelerate.ArBB as ArBB


import Data.Int
-- import Control.Exception
-- import Control.Monad
import Data.Time
import Data.List

-- import System.Random.MWC 
-- import Random -- accelerate-examples/src/common/Random.hs


import Control.Exception
import System.Environment
import System.Mem


sum3 (x,y,z) = x + y + z

sumarr arr = loop 0 mx
 where 
   (0,mx) = IA.bounds arr
   loop !acc 0 = acc + arr IA.! 0
   loop !acc i = loop (acc + arr IA.! i) (i-1)


sumvec :: Num a => Vector a -> a
sumvec vec = loop 0 (mx - 1)
 where 
--   mx = Acc.arraySize$ Acc.arrayShape vec
   mx :: Int = Acc.arraySize$ Acc.arrayShape vec
--   Z :. mx = Acc.arraySize$ Acc.arrayShape vec

   loop !acc 0 = acc + Acc.indexArray vec (Z :. 0)
   loop !acc i = loop (acc + Acc.indexArray vec (Z :. i)) (i-1)


timeit io = do
  t1 <- getCurrentTime
  r  <- io
  t2 <- getCurrentTime
  performGC
  return (r, diffUTCTime t2 t1) 


main = do
-- withSystemRandom $ \gen -> do
  args <- getArgs 
  let size = case args of []  -> 1000000
			  [s] -> read s

  putStrLn$ "Testing generation of arrays of size "++ show size

  
  do putStrLn "Test 1: Create a haskell list."  
     (sum, tm) <- timeit $ evaluate$ foldl1' (+) ([1..size] :: [Int])
     putStrLn$ "  Done generating and folding data, sum "++ show sum ++": " ++ show tm


  ----------------------------------------
  do putStrLn "\nTest 2: Create a Haskell array."  
     -- I continue to be shocked that IArray doesn't offer other ways to build arrays:
     -- They would always be optional, and would enable much more efficiency if deforestation fails.
     let arr = A.listArray (0,size-1) ([1..size] :: [Int])
     (sum, tm) <- timeit $ evaluate$ sumarr arr
     putStrLn$ "  Done generating and folding data, sum "++ show sum ++": " ++ show tm


  ----------------------------------------
  do putStrLn "\nTest 3: Create an Accelerate array from a Haskell array" 
     let arr = A.listArray (0,size-1) ([1..size] :: [Int])
         vec = Acc.fromIArray arr :: Vector Int
--     (sz, tm) <- timeit $ evaluate$ Acc.arrayShape vec
     (sz, tm) <- timeit $ evaluate$ sumvec vec
     putStrLn$ "  Done, result "++ show sz ++": " ++ show tm


  ----------------------------------------
  do putStrLn "\nTest 4: Create an Accelerate array from a Haskell list" 
     let vec = Acc.fromList (Sugar.listToShape [size]) [1..size] :: Vector Int
     (sz, tm) <- timeit $ evaluate$ Acc.arrayShape vec
     putStrLn$ "  Done, result "++ show sz ++": " ++ show tm


  putStrLn ""
  putStrLn "Next test large arrays of tuples."
  putStrLn "================================================================================"



  -- t_g_1 <- getCurrentTime
  -- v_sp <- randomUArrayR (3,30)     gen n
  -- v_os <- randomUArrayR (1,100)    gen n
  -- v_oy <- randomUArrayR (0.25, 10) gen n 
  -- t_g_2 <- getCurrentTime
  -- a_psy <- evaluate$  Acc.fromList (Sugar.listToShape [n]) $ zip3 (elems v_sp) (elems v_os) (elems v_oy)
  -- t_g_3 <- getCurrentTime
  
--   putStrLn$ "Zip stage took: " ++ show (diffUTCTime t_g_3 t_g_2) 

--   t_p_1 <- getCurrentTime
--   r' <-  evaluate$ ArBB.run (blackscholesAcc a_psy)
--   t_p_2 <- getCurrentTime
--   r0' <- evaluate$ Interp.run (blackscholesAcc a_psy) 
--   t_p_3 <- getCurrentTime 

-- --  putStrLn$ "BlackScholes: " ++ if checkResult r r0 == [] then "Passed" else "failed "
--   putStrLn$ "Time ArBB : " ++ ( show (diffUTCTime t_p_2 t_p_1) )  
--   putStrLn$ "Time InterP : " ++ ( show (diffUTCTime t_p_3 t_p_2) )  

--   putStrLn$ show$ take 5$ Prelude.zip (toList r') (toList r0')
-- --   putStrLn$ show$ toList r0'
  
--   return ()
