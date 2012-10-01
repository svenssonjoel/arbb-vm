{-# LANGUAGE ScopedTypeVariables#-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr 
import Foreign.ForeignPtr

import Data.Word

main = arbbSession$ do 
     sty <- getScalarType_ ArbbI32
     dty <- getDenseType_ sty 1
     size_t <- getScalarType_ ArbbUsize
     dsize_t <- getDenseType_ size_t 1


     addOne <- funDef_ "add" [sty] [sty] $ \ [out] [a] -> do
        one <- int32_ 1
        op_ ArbbOpAdd [out] [a,one]
  

     -- Takes an array as input for no reason 
     maper <- funDef_ "id" [dty] [dty] $ \ [out] [inp] -> do        
        map_ addOne [out] [inp] 
   
     liftIO$ putStrLn "Done compiling function, now executing..."

   
     withArray_ [1001,1002,1003,1004,1005,1006,1007,1008,1009,1010 ::Word32] $ \ inp -> 
   --    withArray_ (replicate 10 0 :: [Word32]) $ \ out -> 
       do
       
       -- inb <- createDenseBinding_ (castPtr inp) 1 [10] [4]
       --  outb <- createDenseBinding_ (castPtr out) 1 [10] [4]
         
        --inb <- getBindingNull_
        --outb <- getBindingNull_ 
         
        gin <- createGlobal_nobind_ dty "input" --inb
        gout <- createGlobal_nobind_ dty "output" --outb
        gIm1 <- createGlobal_nobind_ dty "output" --outb 
        vIm1 <- variableFromGlobal_ gIm1
        gIm2 <- createGlobal_nobind_ dty "output" --outb
        vIm2 <- variableFromGlobal_ gIm2
       
       
       
        vin <- variableFromGlobal_ gin
        vout <- variableFromGlobal_ gout
       


        n <- usize_ 10
        opDynamicImm_ ArbbOpAlloc [vin] [n] 
        (inputs,pitches) <- mapToHost_ vin ArbbWriteOnlyRange
        liftIO$ copyBytes inputs (castPtr inp) 40         
        
    -- Repeated execution 
    -- Why does this test work! 
      
        execute_ maper [vIm1] [vin]    
     
        execute_ maper [vIm2] [vIm1]  

        execute_ maper [vout] [vIm2]  
                 
        (out,pitches) <- mapToHost_ vout ArbbReadOnlyRange
         
        result <- liftIO $ peekArray 10 (castPtr out :: Ptr Word32) 
        liftIO$ putStrLn $ show result
         
        
     
    --  result :: Word32 <- readScalar_ y 
     
    -- liftIO$ putStrLn$ "Result2: "++ show result  