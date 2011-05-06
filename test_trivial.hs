

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

-- import Foreign.Marshal.Array (withArray)
import Foreign.Ptr 

-- import C2HS

-- Gradually building up to be able to do Test_Simple1.hs:

main = runReproducer$
  do 
     ctx <- getDefaultContext 
     t   <- getScalarType ctx ArbbF32
     fnt <- getFunctionType ctx [t] [t,t,t] 
     myfun <- beginFunction ctx fnt "add" 0
     a     <- getParameter myfun 0 0 
     b     <- getParameter myfun 0 1
     c     <- getParameter myfun 0 2
     d     <- getParameter myfun 1 0 
     op myfun ArbbOpAdd [d] [a,b]
     op myfun ArbbOpMul [d] [c,d]
     endFunction myfun
     compile myfun
     binding <- getBindingNull 

     -- -- This part gets messy! 
     -- -- TODO: Clean up! 
     withArray [10.0, 20.0,30.0 :: Float] $ \ input -> 
        do 
           g1 <- createConstant ctx t (castPtr input)
           g2 <- createConstant ctx t (plusPtr (castPtr input) 4) 
           g3 <- createConstant ctx t (plusPtr (castPtr input) 8)
           v1 <- variableFromGlobal ctx g1;
           v2 <- variableFromGlobal ctx g2;
           v3 <- variableFromGlobal ctx g3;
           r  <- createGlobal ctx t "result" binding
           v4 <- variableFromGlobal ctx r 


           execute myfun [v4] [v1,v2,v3]
     --      -- TODO: Figure out how to best access results (of various types) 
           result <- readScalarOfSize 4 ctx v4 :: IO Float
           putStrLn (show result)       

     putStrLn "Test Done."
