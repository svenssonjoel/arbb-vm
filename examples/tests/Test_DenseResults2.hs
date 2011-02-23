{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import Data.Word
--import C2HS

{- 
   Update.. 
-}

main = arbbSession$ do 
     sty <- getScalarType_ ArbbI32
     dty <- getDenseType_ sty 1
     size_t <- getScalarType_ ArbbUsize
     dsize_t <- getDenseType_ size_t 1

     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [i1,i2] -> do 
        op_ ArbbOpAdd [out] [i1,i2] 
      
     -- Create a new array using newVector and then fill it 
     arrayUPD <- funDef_ "id" [dty] [dty] $ \ [out] [inp] -> do 
        
        indices <- createLocal_ dty "indices"
        
        newArr  <- createLocal_ dty "new!"           
        
        one <- int32_ 1
        ten <- usize_ 10 
                   
        opDynamic_ ArbbOpNewVector [newArr] [ten] 
          
        opDynamic_ ArbbOpIndex [indices] [one,ten,one]      
        
        map_ add [newArr] [indices,inp]    -- Both these calls 
        --op_ ArbbOpAdd [newArr] [indices,inp] -- give SAME result  

        op_ ArbbOpCopy [out] [newArr] 
        
     -- Or skip creating new array using newVector (BOTH WORKS) 
     arrayUPD2 <- funDef_ "id" [dty] [dty] $ \ [out] [inp] -> do 
        
        indices <- createLocal_ dty "indices"
        
        newArr  <- createLocal_ dty "new!"           
        
        one <- int32_ 1
        ten <- usize_ 10 
                      
        opDynamic_ ArbbOpIndex [indices] [one,ten,one]      
        
        map_ add [newArr] [indices,inp]    -- Both these calls 
        --op_ ArbbOpAdd [newArr] [indices,inp] -- give SAME result  

        op_ ArbbOpCopy [out] [newArr] 
        
     liftIO$ putStrLn "Done compiling function, now executing..."
 
   
     withArray_ [100,100,100,100,100,100,100,100,100,100 ::Word32] $ \ inp -> 
      withArray_ (replicate 10 0 :: [Word32]) $ \ out -> 
       do
       
        inb <- createDenseBinding_ (castPtr inp) 1 [10] [4]
        outb <- createDenseBinding_ (castPtr out) 1 [10] [4]
       
        gin <- createGlobal_ dty "input" inb
        gout <- createGlobal_ dty "output" outb
       
        vin <- variableFromGlobal_ gin
        vout <- variableFromGlobal_ gout
      
        execute_ arrayUPD2 [vout] [vin]     
         
         
        result <- liftIO $ peekArray 10 (castPtr out :: Ptr Word32) 
        liftIO$ putStrLn $ show result
         
        
     