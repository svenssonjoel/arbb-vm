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
     bt     <- getScalarType_ ArbbBoolean
     
     one    <- int32_ 1 
     zero   <- int32_ 0 
     ten    <- int32_ 10 
     tusentjugofyra <- int32_ 1024
    
     print_ "Begin emitting function code.."

     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [a,b] -> do
        op_ ArbbOpAdd [out] [a,b]
{-       
     reduce2 <- funDef_ "reduceSpcl" [sty] [dty] $ \ [out] [inp] -> do 
        len <- createLocal_ size_t "length"     
        res <- createLocal_ sty "result"
        in1 <- createLocal_ sty "inputToCall"  
            
        op_ ArbbOpLength [len] [inp]
        op_ ArbbOpCast   [in1] [len] 
   
        call_ add [res] [in1,in1]
        --op_ ArbbOpAdd   [res] [in1,in1]      
        op_ ArbbOpCopy  [out] [res]  
-}
              
     reduce2 <- funDef_ "reduceSpcl" [sty] [dty] $ \ [out] [inp] -> do 
        len <- createLocal_ size_t "length"     
        res <- createLocal_ sty "result"
        in1 <- createLocal_ sty "inputToCall" 
        pr  <- createLocal_ sty "partialRes"  
        lcntr <- createLocal_ sty "loopcounter"  
        max <- createLocal_ sty "max"   
            
        op_ ArbbOpLength [len] [inp]
        op_ ArbbOpCast   [in1] [len] 
        
        op_ ArbbOpCopy   [lcntr] [zero]
        op_ ArbbOpCopy   [pr] [zero] 
        op_ ArbbOpCopy   [max] [ten]   -- change to 40 for error
         
        while_ 
          ( -- condition
           do 
             cvar <- createLocal_ bt "loopcond"
             op_ ArbbOpLess [cvar] [lcntr,max] 
             return cvar
          ) 
          (-- body 
           do 
            tmp <- createLocal_ sty "ls"
            --index <- createLocal_ size_t "ix"
            --op_ ArbbOpCast [index] [lcntr]
            -- op_ ArbbOpCast [tmp]  [index]
            --op_ ArbbOpExtract [tmp] [inp,index]  
            -- call_ add [tmp] [pr,in1]
            --op_ ArbbOpAdd [pr] [pr,tmp]
            op_ ArbbOpAdd [lcntr] [lcntr,one]
          )
 
        
        --op_ ArbbOpCopy [out] [pr] 
        --op_ ArbbOpCopy  [out] [max]  
        op_ ArbbOpCopy  [out] [lcntr]   



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
    
     