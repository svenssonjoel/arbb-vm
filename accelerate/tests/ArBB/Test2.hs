
-- ghci -i.. -Iinclude tests/ArBB/Test2.hs -ltbb -larbb


import Data.Array.Unboxed
import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.AST

import qualified Data.Array.Accelerate.Smart as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar


import           Data.Array.Accelerate.ArBB hiding (run)
import qualified Data.Array.Accelerate.ArBB as ArBB 

import Intel.ArbbVM
import Intel.ArbbVM.Convenience

import Data.Int

import Prelude hiding (map,zipWith)

incr xs = 
     let xs' = use xs
     in map (*3) xs'


sumUp xs = 
     let xs' = use xs
     in fold (+) 0 xs'

pairSum xs ys = 
   let xs' = use xs
       ys' = use ys 
   in zipWith (+) xs' ys'



input :: Data.Array.Accelerate.Array Sugar.DIM1 Int
input = fromList  (Sugar.listToShape [1024]) [1..1024 :: Int]


input2 :: Data.Array.Accelerate.Array Sugar.DIM1 Int
input2 = fromList  (Sugar.listToShape [1024]) (Prelude.replicate 1024 1024 :: [Int]) 

input3 :: Data.Array.Accelerate.Array Sugar.DIM2 Int
input3 = fromList  (Sugar.listToShape [512,2]) (Prelude.replicate 1024 1 :: [Int])

input4 :: Data.Array.Accelerate.Array Sugar.DIM2 Int
input4 = fromList  (Sugar.listToShape [512,2]) (Prelude.replicate 1024 2 :: [Int])

input5 :: Data.Array.Accelerate.Array Sugar.DIM2 Int
input5 = fromList  (Sugar.listToShape [512,2]) ([0..1023 :: Int])





apa = -- arbbSession$ do 
  let f = incr input
  in ArBB.run f 

bepa = 
  let f = pairSum input input2 
  in ArBB.run f  

cepa =
   let xs' = use input
       prg = zipWith (+) xs' xs'
   in  ArBB.run prg    

-- What happens with two dimensional input 
-- This one works because the higher dimensioned 
-- array is stored as a single linear one. 
--  - Will it start breaking down for the operations such as fold
depa = 
  let f = incr input3 -- input3 is two dimensional 
  in ArBB.run f  


depa2 = 
   let xs' = use input3
   in ArBB.run xs'

depa3 = 
   let xs = use input4
   in ArBB.run xs


-- TODO: Turn float
-- TODO: Look at def of Vector and Scalar
dotpAcc :: Vector Int -> Vector Int -> Sugar.Acc (Scalar Int)
dotpAcc xs ys
  = let
      xs' = use xs
      ys' = use ys
    in
    fold (+) 0 (zipWith (*) xs' ys')

runDotP = do 
   ArBB.run$ dotpAcc input input

fold_plus = do 
   ArBB.run$ fold (+) 0 (use input)

fold_plus1 = do 
   ArBB.run$ fold (+) 0 (use input3)

fold_plus2 = do 
   ArBB.run$ fold (+) 0 (use input4)

fold_plus3 = do 
   ArBB.run$ fold (+) 0 (use input5)

fold_plus_Interp = do 
   run$ fold (+) 0 (use input5)


main = do
     putStrLn$ show depa   
{-
     putStrLn "USING: Immediate ArBB Backend" 
     putStrLn$ show apa
     putStrLn "Testing zipWith" 
     putStrLn$ show bepa
     putStrLn "Testing zipWith Same input twice" 
     putStrLn$ show cepa
-}