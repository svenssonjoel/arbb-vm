{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import Data.Word
--import C2HS

{- see ArBB function arrayIx2 for using Index -}

main = arbbSession$ do 
     sty <- getScalarType_ ArbbI32
     dty <- getDenseType_ sty 1
     size_t <- getScalarType_ ArbbUsize
     dsize_t <- getDenseType_ size_t 1


     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [a,b] -> do
        op_ ArbbOpAdd [out] [a,b]

    
     arrayID <- funDef_ "id" [dty] [dty] $ \ [out] [inp] -> do 
        op_ ArbbOpCopy [out] [inp]      
    

     -- Takes an array as input for no reason 
     arrayIX <- funDef_ "id" [dty] [dty] $ \ [out] [inp] -> do 
        
        indices <- createLocal_ dsize_t "indices"
        res     <- createLocal_ dty "result" 
        
        one <- usize_ 1
        ten <- usize_ 10 
                              
        opDynamic_ ArbbOpIndex [indices] [one,ten,one]      
    
        -- op_ ArbbOpCopy [out] [indices] -- Fails TYPE MISMATCH
        op_ ArbbOpCast [out] [indices] -- Works (Cast the whole thing!) 

     -- Takes an array as input for no reason       
     arrayIX2 <- funDef_ "id" [dty] [dty] $ \ [out] [inp] -> do 
        
        indices <- createLocal_ dty "indices"
        
        one <- int32_ 1
        ten <- usize_ 10 
                              
        opDynamic_ ArbbOpIndex [indices] [one,ten,one]      
    
        op_ ArbbOpCopy [out] [indices] -- Works!
        --op_ ArbbOpCast [out] [indices] -- Works (but not needed) 


        
     liftIO$ putStrLn "Done compiling function, now executing..."
 
        
     a <- int32_ 2
     b <- int32_ 3
 
     binding <- getBindingNull_
     g       <- createGlobal_ sty "res" binding
     y       <- variableFromGlobal_ g
   
     --execute_ reduce2 [y] [v1]
     execute_ add [y] [a,b]

     withArray_ [1001,1002,1003,1004,1005,1006,1007,1008,1009,1010 ::Word32] $ \ inp -> 
      withArray_ (replicate 10 0 :: [Word32]) $ \ out -> 
       do
       
        inb <- createDenseBinding_ (castPtr inp) 1 [10] [4]
        outb <- createDenseBinding_ (castPtr out) 1 [10] [4]
       
        gin <- createGlobal_ dty "input" inb
        gout <- createGlobal_ dty "output" outb
       
        vin <- variableFromGlobal_ gin
        vout <- variableFromGlobal_ gout
       
        --execute_ arrayID [vout] [vin]
        --execute_ arrayIX [vout] [vin] 
        execute_ arrayIX2 [vout] [vin]     
         
         
        result <- liftIO $ peekArray 10 (castPtr out :: Ptr Word32) 
        liftIO$ putStrLn $ show result
         
        
     
     result :: Word32 <- readScalar_ y 
     
     liftIO$ putStrLn$ "Result2: "++ show result  