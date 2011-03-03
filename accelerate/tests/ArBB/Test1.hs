

import Data.Array.Unboxed
import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.AST

import qualified Data.Array.Accelerate.Smart as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar


import Data.Array.Accelerate.ArBB
import Intel.ArbbVM
import Intel.ArbbVM.Convenience

import Prelude hiding (map)

incr xs = 
     let xs' = use xs
     in map (+1) xs'


sumUp xs = 
     let xs' = use xs
     in fold (+) 0 xs'





input = ((fromList 10 [1..10 :: Int]) :: Data.Array.Accelerate.Array Int Int) 

input2 = ((fromList 10 [1..10 :: Float]) :: Data.Array.Accelerate.Array Int Float) 


apa = arbbSession $ do 
    f <- testa (incr input)
    str <- serializeFunction_ f
    liftIO$ putStrLn (getCString str)

bepa = arbbSession$ do           
   let f = (incr input) 
   runArBB (Sugar.convertAcc f)
