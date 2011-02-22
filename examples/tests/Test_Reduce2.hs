{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS



main = arbbSession$ do 
     sty    <- getScalarType_  ArbbI32
     size_t <- getScalarType_  ArbbUsize       
     dty    <- getDenseType_ sty 1 

     ten <- const_ ArbbI32 (10::Int32)
     
     --reduce <- funDef_ "reduceAdd" [dty] [dty] $ \ [out] [inp] -> do
     --   opDynamic_ ArbbOpAddReduce  [out] [inp]

     print_ "Begin emitting function code.."

-- Not so strange anymore --- 
#if 1
     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [a,b] -> do
        op_ ArbbOpAdd [out] [a,b]
       

#endif
     
     reduce2 <- funDef_ "reduceSpcl" [sty] [dty] $ \ [out] [inp] -> do 
        len <- createLocal_ size_t "length"     
        res <- createLocal_ sty "result"
        in1 <- createLocal_ sty "inputToCall"  
            
        op_ ArbbOpLength [len] [inp]
        op_ ArbbOpCast   [in1] [len] 
   
        call_ add [res] [in1,in1]
        --op_ ArbbOpAdd   [res] [in1,in1]      
        op_ ArbbOpCopy  [out] [res]  
      
        
-- ERROR DOESN'T SHOW UNTIL EXECUTE                 

     liftIO$ putStrLn "Done compiling function, now executing..."

     i_data  <- liftIO$ newArray (replicate 1024 1 :: [Word32])
     o_data  <- liftIO$ newArray [0 :: Word32]
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
     

     result2 :: Word32 <- readScalar_ y 

     
  --   liftIO$ putStrLn$ "Result: "++ show result
     liftIO$ putStrLn$ "Result2: "++ show result2   
    
     