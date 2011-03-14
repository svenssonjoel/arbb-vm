
-- ghci -i.. -Iinclude tests/ArBB/Test1.hs -ltbb -larbb


import Data.Array.Unboxed
import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.AST

import qualified Data.Array.Accelerate.Smart as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar


import Data.Array.Accelerate.ArBB
import Intel.ArbbVM
import Intel.ArbbVM.Convenience

import Data.Int

import Prelude hiding (map)

incr xs = 
     let xs' = use xs
     in map (\x -> x+1) xs'


sumUp xs = 
     let xs' = use xs
     in fold (+) 0 xs'





input = ((fromList  (Sugar.listToShape [1024]) [1..1024 :: Int]) :: Data.Array.Accelerate.Array Sugar.DIM1  Int) 

--input2 = ((fromList [1..10 :: Float]) :: Data.Array.Accelerate.Array Int Float) 


--apa = arbbSession $ do 
--    f <- testa (incr input)
--    str <- serializeFunction_ f
--    liftIO$ putStrLn (getCString str)

{-
bepa = arbbSession$ do           
   let f = incr input
   compileArBB (Sugar.convertAcc f)
   genArBB (Sugar.convertAcc f)
-}

{-
cepa = arbbSession$ do 
  let f = incr input
  dummies <- executeArBB (Sugar.convertAcc f)
  return ()
-}

depa = arbbSession$ do 
  let f = incr input
  executeArBB (Sugar.convertAcc f)
  

main =  depa