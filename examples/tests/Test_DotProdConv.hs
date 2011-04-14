{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import Data.Word

import Data.Time

{- 
   DotProduct using convenience layer 
-}

gen_DotProd sty = do 
   dty <- getDenseType_ sty 1 
   fun <- funDef_ "dotProd" [sty] [dty,dty] $ \ [out] [in1,in2] -> do 
      tmp <- createLocal_ dty "tmp"  
      op_ ArbbOpMul [tmp] [in1,in2]     
      opDynamic_ ArbbOpAddReduce [out] [tmp] 
   return fun   

main = arbbSession$ do 
     sty <- getScalarType_ ArbbF32
     dty <- getDenseType_ sty 1  
          
     dotprod <- funDef_ "dotProd" [sty] [dty,dty] $ \[out] [in1,in2] -> do 
       tmp <- createLocal_ dty "tmp"  
       op_ ArbbOpMul [tmp] [in1,in2]     
       opDynamic_ ArbbOpAddReduce [out] [tmp] 
 
    withArray_ (replicate (2^24) 1 :: [Float]) $ \ in1 ->
      withArray_ (replicate (2^24) 1 :: [Float]) $ \ in2 -> do
        inb1 <- createDenseBinding_ (castPtr in1) 1 [2^24] [4] 
        inb2 <- createDenseBinding_ (castPtr in2) 1 [2^24] [4] 
        
        gin1 <- createGlobal_ dty "gin1" inb1
        gin2 <- createGlobal_ dty "gin2" inb2 
        
        vin1 <- variableFromGlobal_ gin1
        vin2 <- variableFromGlobal_ gin2 
        
        outb <- getBindingNull_ 
        g    <- createGlobal_ sty "res" outb
        y    <- variableFromGlobal_ g 
        
        execute_ dotprod [y] [vin1,vin2] 
        result :: Float <- readScalar_ y
               
        liftIO$ putStrLn $ show result                
       