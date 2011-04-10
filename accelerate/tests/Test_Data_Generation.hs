
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

import Prelude as P
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
   mx :: Int = Acc.arraySize$ Acc.arrayShape vec
   loop !acc 0 = acc + Acc.indexArray vec (Z :. 0)
   loop !acc i = loop (acc + Acc.indexArray vec (Z :. i)) (i-1)

sumprvec :: Num a => Vector (a,a) -> a
sumprvec vec = loop 0 (mx - 1)
 where 
   mx :: Int = Acc.arraySize$ Acc.arrayShape vec
   loop !acc 0 = let (x,y) = Acc.indexArray vec (Z :. 0) in 
		 acc + x + y
   loop !acc i = let (x,y) = Acc.indexArray vec (Z :. i) in
		 loop (acc + x + y) (i-1)


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
     (sz, tm) <- timeit $ evaluate$ sumvec vec
     putStrLn$ "  Done, result "++ show sz ++": " ++ show tm


  ----------------------------------------
-- This one takes forever, disabling for now:
#ifdef HORRIBLEONE
  do putStrLn "\nTest 4: Create an Accelerate array from a Haskell list" 
     let vec = Acc.fromList (Sugar.listToShape [size]) [1..size] :: Vector Int
     (sz, tm) <- timeit $ evaluate$ Acc.arrayShape vec
     putStrLn$ "  Done, result "++ show sz ++": " ++ show tm
#endif


  putStrLn ""
  putStrLn "Next test large arrays of tuples."
  putStrLn "================================================================================"


  ----------------------------------------
  do putStrLn "\nTest 5: Create an Accelerate array from a Haskell Array-of-pairs" 
     let arr = A.listArray (0,size-1) (P.map (\x -> (x,x)) [1..size])
         vec = Acc.fromIArray arr :: Vector (Int,Int)
     (sz, tm) <- timeit $ evaluate$ Acc.arrayShape vec
     putStrLn$ "  Done, result "++ show sz ++": " ++ show tm

---------------------------------------------------
-- After watching it take ten seconds to make an Acc array from a
-- haskell array on a 3.33ghz nehalem, it just ran very quickly on my
-- mac LAPTOP:

    -- $ ./Test_Data_Generation.exe 
    -- Testing generation of arrays of size 1000000
    -- Test 1: Create a haskell list.
    --   Done generating and folding data, sum 1784293664: 0.016479s

    -- Test 2: Create a Haskell array.
    --   Done generating and folding data, sum 1784293664: 0.11281s

    -- Test 3: Create an Accelerate array from a Haskell array
    --   Done, result 1784293664: 0.643427s

    -- Next test large arrays of tuples.
    -- ================================================================================

    -- Test 5: Create an Accelerate array from a Haskell Array-of-pairs
    --   Done, result Z :. 1000000: 0.742691s



