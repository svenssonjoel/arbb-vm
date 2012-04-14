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
  BJS: Trying to pinpoint where creating functions "breaks"
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

     myAdd <- funDef_ "myAdd" [sty] [sty,sty] $ \ [out] [i1,i2] -> do
       op_ ArbbOpAdd [out] [i1,i2]                      
     
     add <- funDef_ "add" [sty] [sty] $ \ [out] [a] -> do

       call_ myAdd [out] [a,one]
      
                 
     
     fun <- funDef_ "fun" [sty] [dty] $ \ [out] [inp] -> do 
        len <- createLocal_ size_t "name"     
        res <- createLocal_ sty "name"
        in1 <- createLocal_ dty "name"  
       

        map_ add [in1] [inp] 
   
        opDynamic_ ArbbOpAddReduce [res] [in1]

        op_ ArbbOpCopy  [out] [res]  
  

     liftIO$ putStrLn "Done compiling function, now executing..."

     -- binding <- getBindingNull_
     g       <- createGlobal_nobind_ sty "res" -- binding   
     y       <- variableFromGlobal_ g
     liftIO$ putStrLn "e" 

     -- input data
     i_d  <- liftIO$ newArray (replicate 1024 2 :: [Word32])         
      
     --bin <- getBindingNull_
     g_apa <- createGlobal_nobind_ dty "in" -- bin
     v1 <- variableFromGlobal_ g_apa 
    
     opDynamicImm_ ArbbOpAlloc [v1] [tusentjugofyra]
     
     m_ptr <- mapToHost_ v1 [1] ArbbReadWriteRange
     liftIO$ copyBytes m_ptr (castPtr i_d) 4096
     
     -- now run the computations
     execute_ fun [y] [v1]     
     liftIO$ putStrLn "f" 
     

     result :: Word32 <- readScalar_ y 

     liftIO$ putStrLn$ "Result2: "++ show result  
    
     