{-# LANGUAGE CPP, ScopedTypeVariables #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS
{- 

  Changes:
  BJS: Changed this test on Apr-21-2011 to correspond to 
       new information about the ArBB is_remote parameter to functions
    
       The error message recieved about this wrapper issue is not always pleasant: 
       "ArbbVMException ArbbErrorInternal "Internal error: 
       CTE_ILLEGAL_MEMORY_OPERAND ILLEGAL_MEM_OP: Access the 
       illegal memory address  => Accessing not mapped address 
       [0x64] at IP[0x7f2a9439b021] IP[0x7f2a9439b021]"

-}  


main = arbbSession$ do 
     sty   <- getScalarType_  ArbbI32
     
     fun1 <- funDefCallable_ "fun1" [sty] [sty] $ \ [out] [inp] -> do

        fun2 <- funDefCallable_ "fun2" [sty] [sty] $ \ [out] [inp] -> do
	   tmp <- doarith_ ArbbI32 (V inp + 1)
	   copy_ out tmp

        ten <- const_ ArbbI32 (10::Int32)
	call_ fun2 [out] [inp]
	op_ ArbbOpAdd  [out] [out,ten]

        
     fun <- funDef_ "fun" [sty] [sty] $ \ [out] [inp] -> do 
        call_ fun1 [out] [inp]                      
            
     liftIO$ putStrLn "Done compiling function, now executing..."
     x       <- const_ ArbbI32 (100::Int32)
     binding <- getBindingNull_
     g       <- createGlobal_ sty "res" binding
     y       <- variableFromGlobal_ g

     execute_ fun [y] [x]

     result :: Int32 <- readScalar_ y 
     liftIO$ putStrLn$ "Result from function application: "++ show result
