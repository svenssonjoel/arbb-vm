{-# LANGUAGE CPP, ScopedTypeVariables #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS

{- 

  Changes:
  BJS: Updated on Apr-22-2011 to fit with new Convenience.hs 
       (This change undoes the change below) 

  BJS: Changed this test on Apr-21-2011 to correspond to 
       new information about the ArBB is_remote parameter to functions
       

-}  

main = arbbSession$ do 
     sty <- getScalarType_  ArbbI32
     ten <- const_ ArbbI32 (10::Int32)
     one <- const_ ArbbI32 (1 ::Int32)
     x   <- const_ ArbbI32 (100::Int32) 
     
     fun2 <- funDef_ "fun2" [sty] [sty,sty] $ \ [out] [i1,i2] -> do
       	op_ ArbbOpAdd  [out] [i1,i2]


     fun <- funDef_ "fun1" [sty] [sty] $ \ [out] [inp] -> do
        in1 <- createLocal_ sty "in1"
        in2 <- createLocal_ sty "in2"
        res <- createLocal_ sty "res"  
        
	op_ ArbbOpCopy [in1] [inp]
	op_ ArbbOpCopy [in2] [ten]
  
	call_ fun2 [res] [in1,in2]
	
        op_ ArbbOpCopy  [out] [res]
        

     liftIO$ putStrLn "Done compiling function, now executing..."
    
     binding <- getBindingNull_
     g       <- createGlobal_ sty "res" binding
     y       <- variableFromGlobal_ g
     

     
     execute_ fun [y] [x]

     result :: Int32 <- readScalar_ y 
     liftIO$ putStrLn$ "Result from function application: "++ show result
