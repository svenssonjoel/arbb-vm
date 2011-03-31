{-# Language FlexibleContexts, CPP #-}


module Main where 

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
-- import qualified Data.Array.Accelerate.CUDA  

import Data.Int
import Control.Exception
import Control.Monad
import Data.Time

import System.Random.MWC 
import Random -- accelerate-examples/src/common/Random.hs


n = 1000

pairtest :: Vector (Float, Float) -> Acc (Vector Float)
pairtest xs = Acc.map go (Acc.use xs)
  where 
     go x = 
       let (a,b) = Acc.unlift x 
       in a + b

main = withSystemRandom $ \gen -> do
  putStrLn "Generating input data..." 
  t_g_1 <- getCurrentTime
  v_sp <- randomUArrayR (3,30)     gen n
  v_os <- randomUArrayR (1,100)    gen n
  a_psy <- evaluate$  Acc.fromList (Sugar.listToShape [n]) $ Prelude.zip (elems v_sp) (elems v_os)
  t_g_2 <- getCurrentTime

  putStrLn$ "Done generating input data: " ++ ( show (diffUTCTime t_g_2 t_g_1) )  
 
  t_p_1 <- getCurrentTime
  r' <-  evaluate$ ArBB.run (pairtest a_psy)
  t_p_2 <- getCurrentTime
--  r0' <- evaluate$ Interp.run (blackScholes apsy) 
  t_p_3 <- getCurrentTime 

--  putStrLn$ "BlackScholes: " ++ if checkResult r r0 == [] then "Passed" else "failed "
  putStrLn$ "Time ArBB : " ++ ( show (diffUTCTime t_p_2 t_p_1) )  
--  putStrLn$ "Time InterP : " ++ ( show (diffUTCTime t_p_3 t_p_2) )  
  return ()
