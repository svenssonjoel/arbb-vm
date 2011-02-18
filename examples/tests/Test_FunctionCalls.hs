{-# LANGUAGE CPP, ScopedTypeVariables #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS


main = arbbSession$ do 
     sty   <- getScalarType_  ArbbI32
     
     fun1 <- funDef_ "fun1" [sty] [sty] $ \ [out] [inp] -> do

        fun2 <- funDef_ "fun2" [sty] [sty] $ \ [out] [inp] -> do
           one <- const_ ArbbI32 (1 ::Int32)
	   op_ ArbbOpCopy [out] [inp]
	   op_ ArbbOpAdd  [out] [out,one]

        ten <- const_ ArbbI32 (10::Int32)
	call_ fun2 [out] [inp]
	op_ ArbbOpAdd  [out] [out,ten]

     liftIO$ putStrLn "Done compiling function, now executing..."
     x       <- const_ ArbbI32 (100::Int32)
     binding <- getBindingNull_
     g       <- createGlobal_ sty "res" binding
     y       <- variableFromGlobal_ g

     execute_ fun1 [y] [x]

     result :: Int32 <- readScalar_ y 
     liftIO$ putStrLn$ "Result from function application: "++ show result
