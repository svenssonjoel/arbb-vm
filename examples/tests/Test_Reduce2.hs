{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

{-  
    Entirely sequential reduction in ArBB
-} 
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

     print_ "Begin emitting function code.."

     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [a,b] -> do
        op_ ArbbOpAdd [out] [a,b]
              
     reduce2 <- funDef_ "reduceSpcl" [sty] [dty] $ \ [out] [inp] -> do 
        len <- createLocal_ size_t "length"     
        in1 <- createLocal_ sty "inputToCall" 
        pr  <- createLocal_ sty "partialRes"  
        lcntr <- createLocal_ sty "loopcounter" 
       
        
        -- Obtain length and cast into I32 type
        op_ ArbbOpLength [len] [inp]
        op_ ArbbOpCast   [in1] [len]
   
        
        -- Clear loop counter and running sum 
        op_ ArbbOpCopy   [lcntr] [zero]
        op_ ArbbOpCopy   [pr] [zero] 
    
         
        while_ 
          ( -- condition
           do 
             cvar <- createLocal_ bt "loopcond"
             op_ ArbbOpLess [cvar] [lcntr,in1] 
             return cvar
          ) 
          (-- body 
           do 
            tmp <- createLocal_ sty "ls"
            index <- createLocal_ size_t "ix"
            
            op_ ArbbOpCast [index] [lcntr]
           
            opDynamic_ ArbbOpExtract [tmp] [inp, index]  
          
            call_ add [pr] [pr,tmp]
            op_ ArbbOpAdd [lcntr] [lcntr,one]
          )
 
        
        op_ ArbbOpCopy [out] [pr] 
 



     liftIO$ putStrLn "Done compiling function, now executing..."

     i_data  <- liftIO$ newArray (replicate (2^22) 1 :: [Word32]) 
     i_array <- createDenseBinding_ (castPtr i_data) 1 [2^22] [4]

  
   
     g_in  <- createGlobal_  dty "in" i_array;       
   
     v1 <- variableFromGlobal_ g_in; 
 
     binding <- getBindingNull_
     g       <- createGlobal_ sty "res" binding
     y       <- variableFromGlobal_ g
   
     execute_ reduce2 [y] [v1]     
   
     result2 :: Word32 <- readScalar_ y 
     
     liftIO$ putStrLn$ "Result2: "++ show result2   
    
     