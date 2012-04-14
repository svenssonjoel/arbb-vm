{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr 
import Foreign.ForeignPtr

import Data.Word

{- 
   Note: 
    BJS April-21-2011: This test does not wrap the function 
    to be executed. (as required !!, but still works??) 
    
-} 

main = arbbSession$ do 
     sty    <- getScalarType_  ArbbI32
     size_t <- getScalarType_  ArbbUsize       
     dty    <- getDenseType_ sty 1 
     bt     <- getScalarType_ ArbbBoolean
     
     one    <- int32_ 1 
     zero   <- int32_ 0 
     ten    <- int32_ 10 
     tusentjugofyra <- usize_ 1024
    
     print_ "Begin emitting function code.."

   
     i_d  <- liftIO$ newArray (replicate 1024 2 :: [Word32])         
     --i_a <- createDenseBinding_ (castPtr i_d) 1 [1024] [4]

-- looks good ! apply it ! 
     
     -- bin <- getBindingNull_
     g_apa <- createGlobal_nobind_ dty "in" --bin
     v1 <- variableFromGlobal_ g_apa 
     opDynamicImm_ ArbbOpAlloc [v1] [tusentjugofyra]
     
     m_ptr <- mapToHost_ v1 [1] ArbbReadWriteRange
     liftIO$ copyBytes m_ptr (castPtr i_d) 4096
     
     --g_apa  <- createGlobal_  dty "in" i_a  
                 
     
     fun <- funDef_ "fun" [sty] [] $ \ [out] [] -> do 

        add <- funDef_ "add" [sty] [sty] $ \ [out] [a] -> do
          one <- int32_ 1 
          op_ ArbbOpAdd [out] [a,one]
  
   
        len <- createLocal_ size_t "name"     
        res <- createLocal_ sty "name"
        in1 <- createLocal_ dty "name"  
       
        -- op_ ArbbOpCopy [in1] [inp]       
        map_ add [in1] [v1] 
   
        opDynamic_ ArbbOpAddReduce [res] [in1]
        --op_ ArbbOpLength [len] [v1]
        --op_ ArbbOpCast   [in1] [len] 
   
        -- NOTE! Here we use fresh locals as inputs and outputs rather
        -- than reusing this function's parameters:
        --call_ add [res] [in1,in1]
        --op_ ArbbOpAdd   [res] [in1,in1]      
        op_ ArbbOpCopy  [out] [res]  
  
        

     liftIO$ putStrLn "Done compiling function, now executing..."

 {- 
     i_data  <- liftIO$ newArray (replicate 1024 1 :: [Word32])
   
     i_array <- createDenseBinding_ (castPtr i_data) 1 [1024] [4]
   
     liftIO$ putStrLn "a"  
    
     g_in  <- createGlobal_  dty "in" i_array;       
   
     liftIO$ putStrLn "b" 
   
     v1 <- variableFromGlobal_ g_in;
   
     liftIO$ putStrLn "c"              
-}  

     -- binding <- getBindingNull_
     g       <- createGlobal_nobind_ sty "res" -- binding   
     y       <- variableFromGlobal_ g
     liftIO$ putStrLn "e" 

     execute_ fun [y] []     
     liftIO$ putStrLn "f" 
     

     result :: Word32 <- readScalar_ y 

     liftIO$ putStrLn$ "Result2: "++ show result  
    
     