{-# LANGUAGE CPP, ScopedTypeVariables #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS



main = arbbSession$ do 
     sty   <- getScalarType_  ArbbI64
     dty   <- getDenseType_ sty 1 

     --reduce <- funDef_ "reduceAdd" [dty] [dty] $ \ [out] [inp] -> do
     --   opDynamic_ ArbbOpAddReduce  [out] [inp]

-- STRANGENESS BEGINS HERE      
     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [a,b] -> do
        op_ ArbbOpAdd [out] [a,b]      
      
     
     reduce2 <- funDef_ "reduceSpcl" [sty] [dty] $ \ [out] [inp] -> do 
        --sizeT <- getScalarType_  ArbbIsize     
      --  len   <- createLocal_ dty "len"
      --  tmp   <- createLocal_ sty "tmp"
        one <- const_ ArbbI64 (1 :: Word64)    
        
      --  op_ ArbbOpLength [len] [inp]
      --  op_ ArbbOpIndex  [tmp] [len,one]
        --op_ ArbbOpCopyLength [tmp] [len]
       
        --call_ add [len] [one,one]
        op_ ArbbOpAdd  [out] [one,one]  
      --  op_ ArbbOpCopy [out] [tmp]
        
        
-- ERROR DOESN'T SHOW UNTIL EXECUTE          

        -- op_ ArbbOpCopy   [out] [len]
       

     liftIO$ putStrLn "Done compiling function, now executing..."

     i_data  <- liftIO$ newArray (replicate 1024 1 :: [Word64])
     o_data  <- liftIO$ newArray [0 :: Word64]
     i_array <- createDenseBinding_ (castPtr i_data) 1 [1024] [8]
     o_array <- createDenseBinding_ (castPtr o_data) 1 [1] [8] 
     liftIO$ putStrLn "a"  
    
     g_in  <- createGlobal_  dty "in" i_array;       
     g_out <- createGlobal_  dty "res" o_array;
     liftIO$ putStrLn "b" 
   
     v1 <- variableFromGlobal_ g_in;
     v2 <- variableFromGlobal_ g_out;       
     liftIO$ putStrLn "c"              
        
  --   execute_ reduce [v2] [v1]
  --   result <- liftIO$ peekArray 1 (castPtr o_data :: Ptr Word32)    
  --   liftIO$ putStrLn "d"      

     binding <- getBindingNull_
     g       <- createGlobal_ sty "res" binding
     y       <- variableFromGlobal_ g
     liftIO$ putStrLn "e" 

     execute_ reduce2 [y] [v1]     
     liftIO$ putStrLn "f" 
     

     result2 :: Word64 <- readScalar_ y 

     
  --   liftIO$ putStrLn$ "Result: "++ show result
     liftIO$ putStrLn$ "Result2: "++ show result2   
    
     