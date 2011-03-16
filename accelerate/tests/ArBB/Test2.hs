
-- ghci -i.. -Iinclude tests/ArBB/Test2.hs -ltbb -larbb


import Data.Array.Unboxed
import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.AST

import qualified Data.Array.Accelerate.Smart as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar


import           Data.Array.Accelerate.ArBBimm hiding (run)
import qualified Data.Array.Accelerate.ArBBimm as ArBB 

import Intel.ArbbVM
import Intel.ArbbVM.Convenience

import Data.Int

import Prelude hiding (map)

incr xs = 
     let xs' = use xs
     in map (\x -> x*2+1) xs'


sumUp xs = 
     let xs' = use xs
     in fold (+) 0 xs'




input :: Data.Array.Accelerate.Array Sugar.DIM1 Int
input = (fromList  (Sugar.listToShape [1024]) [1..1024 :: Int]) 


apa = -- arbbSession$ do 
  let f = incr input
  in ArBB.run f 
  
main = do
     putStrLn$ "USING: Immediate ArBB Backend" 
     putStrLn$ show apa