{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS

{- 

-}  
     

main = arbbSession$ do 
     sty    <- getScalarType_  ArbbI32
     size_t <- getScalarType_  ArbbUsize       
     dty    <- getDenseType_ sty 1 
     bt     <- getScalarType_ ArbbBoolean
     
     one    <- int32_ 1 
     zero   <- int32_ 0 
     ten    <- int32_ 10 
     tusentjugofyra <- int32_ 1024
    
     print_ "Begin emitting function code.."

     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [a,b] -> do
        op_ ArbbOpAdd [out] [a,b]

       
     fun <- funDef_ "fun" [sty] [dty] $ \ [out] [inp] -> do 
        len <- createLocal_ size_t "length"     
        res <- createLocal_ sty "result"
        in1 <- createLocal_ sty "inputToCall"  
            
        op_ ArbbOpLength [len] [inp]
        op_ ArbbOpCast   [in1] [len] 
   
        -- NOTE! Here we use fresh locals as inputs and outputs rather
        -- than reusing this function's parameters:
        call_ add [res] [in1,in1]
        --op_ ArbbOpAdd   [res] [in1,in1]      
        op_ ArbbOpCopy  [out] [res]  
      

     liftIO$ putStrLn "Done compiling function, now executing..."

     i_data  <- liftIO$ newArray (replicate 1024 1 :: [Word32])
   
     i_array <- createDenseBinding_ (castPtr i_data) 1 [1024] [8]
   
     liftIO$ putStrLn "a"  
    
     g_in  <- createGlobal_  dty "in" i_array;       
   
     liftIO$ putStrLn "b" 
   
     v1 <- variableFromGlobal_ g_in;
   
     liftIO$ putStrLn "c"              
 
     binding <- getBindingNull_
     g       <- createGlobal_ sty "res" binding   
     y       <- variableFromGlobal_ g
     liftIO$ putStrLn "e" 

     execute_ fun [y] [v1]     
     liftIO$ putStrLn "f" 
     

     result :: Word32 <- readScalar_ y 

     liftIO$ putStrLn$ "Result2: "++ show result  
    
     