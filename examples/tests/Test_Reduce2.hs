{-# LANGUAGE CPP, ScopedTypeVariables #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS



main = arbbSession$ do 
     sty   <- getScalarType_  ArbbI32
     dty   <- getDenseType_ sty 1 

     --reduce <- funDef_ "reduceAdd" [dty] [dty] $ \ [out] [inp] -> do
     --   opDynamic_ ArbbOpAddReduce  [out] [inp]

-- STRANGENESS BEGINS HERE      
     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [a,b] -> do
        op_ ArbbOpAdd [out] [a,b]      
      
     
     reduce2 <- funDef_ "reduceSpcl" [sty] [dty] $ \ [out] [inp] -> do 
        len <- createLocal_ sty "len"
        op_ ArbbOpLength [len] [inp]
        
        call_ add [out] [len,len]
-- ERROR DOESN'T SHOW UNTIL EXECUTE          

        -- op_ ArbbOpCopy   [out] [len]
       

     liftIO$ putStrLn "Done compiling function, now executing..."

     i_data  <- liftIO$ newArray (replicate 1024 1 :: [Word32])
     o_data  <- liftIO$ newArray [0 :: Word32]
     i_array <- createDenseBinding_ (castPtr i_data) 1 [1024] [4]
     o_array <- createDenseBinding_ (castPtr o_data) 1 [1] [4] 
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
     

     result2 :: Int32 <- readScalar_ y 

     
  --   liftIO$ putStrLn$ "Result: "++ show result
     liftIO$ putStrLn$ "Result2: "++ show result2   
    
     