{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS

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

     myAdd <- funDefCallable_ "myAdd" [sty] [sty,sty] $ \ [out] [i1,i2] -> do
       a1 <- createLocal_ sty "a1" 
       a2 <- createLocal_ sty "a2" 
       r  <- createLocal_ sty "r"
             
       copy_ a1 i1
       copy_ a2 i2
       op_ ArbbOpAdd [r] [a1,a2]                      
       copy_ out r
     
     add <- funDefCallable_ "add" [sty] [sty] $ \ [out] [a] -> do
       one <- int32_ 1 
       --call_ myAdd [out] [a,one] 
       op_ ArbbOpAdd [out] [a,one]

                 
     
     fun' <- funDefCallable_ "fun" [sty] [dty] $ \ [out] [inp] -> do 
        len <- createLocal_ size_t "name"     
        res <- createLocal_ sty "name"
        in1 <- createLocal_ dty "name"  
       

        map_ add [in1] [inp] 
   
        opDynamic_ ArbbOpAddReduce [res] [in1]

        op_ ArbbOpCopy  [out] [res]  
  
     fun <- funDef_ "fun" [sty] [dty] $ \ [out] [inp] -> do 
       call_ fun' [out] [inp]            

     liftIO$ putStrLn "Done compiling function, now executing..."

     binding <- getBindingNull_
     g       <- createGlobal_ sty "res" binding   
     y       <- variableFromGlobal_ g
     liftIO$ putStrLn "e" 

     -- input data
     i_d  <- liftIO$ newArray (replicate 1024 2 :: [Word32])         
      
     bin <- getBindingNull_
     g_apa <- createGlobal_ dty "in" bin
     v1 <- variableFromGlobal_ g_apa 
    
     opDynamicImm_ ArbbOpAlloc [v1] [tusentjugofyra]
     
     m_ptr <- mapToHost_ v1 [1] ArbbReadWriteRange
     liftIO$ copyBytes m_ptr (castPtr i_d) 4096
     
     -- now run the computations
     execute_ fun [y] [v1]     
     liftIO$ putStrLn "f" 
     

     result :: Word32 <- readScalar_ y 

     liftIO$ putStrLn$ "Result2: "++ show result  
    
     