{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS



main = arbbSession$ do 
     sty   <- getScalarType_  ArbbI64
     dty   <- getDenseType_ sty 1 

     --reduce <- funDef_ "reduceAdd" [dty] [dty] $ \ [out] [inp] -> do
     --   opDynamic_ ArbbOpAddReduce  [out] [inp]

     print_ "Begin emitting function code.."

-- STRANGENESS BEGINS HERE      
#if 1
     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [a,b] -> do
        op_ ArbbOpAdd [out] [a,b]      

     -- This function seemed t ostart my troubles with the following error:
     --   Test_Reduce2.exe: ArbbVMException ArbbErrorScoping "Invalid
     --   scope 'inputs' to arbb_op: local created in another
     --   function"

     fresh   <- getScalarType_  ArbbI64
--     ident <- funDef_ "ident" [sty] [sty] $ \ [foo] [bar] -> do
     ident <- funDef_ "ident" [fresh] [fresh] $ \ [foo] [bar] -> do
        copy_ foo bar
#endif
     
     reduce2 <- funDef_ "reduceSpcl" [sty] [dty] $ \ [out] [inp] -> do 
        --sizeT <- getScalarType_  ArbbIsize     
      --  len   <- createLocal_ dty "len"
      --  tmp   <- createLocal_ sty "tmp"
        one <- int64_ 1 
        
      --  op_ ArbbOpLength [len] [inp]
      --  op_ ArbbOpIndex  [tmp] [len,one]
        --op_ ArbbOpCopyLength [tmp] [len]

        -- Trying the same sort of thing that works in Test_FunctionCalls:
	ident <- funDef_ "ident" [sty] [sty] $ \ [foo] [bar] -> copy_ foo bar
        add1 <- funDef_ "add1" [sty] [sty] $ \ [out] [inp] -> do
           one <- int64_ 1
	   op_ ArbbOpCopy [out] [inp]
	   op_ ArbbOpAdd  [out] [out,one]

        call_ add [out] [one,one]
        --call_ add1 [out] [one]
        op_ ArbbOpAdd  [out] [one,one]  
      --  op_ ArbbOpCopy [out] [tmp]
        
-- ERROR DOESN'T SHOW UNTIL EXECUTE                 

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
    
     