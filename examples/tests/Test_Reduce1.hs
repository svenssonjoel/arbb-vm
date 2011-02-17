
-- -----------------------------------------------------------------------------
-- Test_Reduce1 

import Intel.ArbbVM 

import Foreign.Marshal.Array
import Foreign.Ptr 

import C2HS
readScalarOfSize n ctx v = 
    allocaBytes n $ \ptr -> 
       do       
        readScalar ctx v ptr 
        peek (castPtr ptr)

main = do 
     ctx <- getDefaultContext 
     st   <- getScalarType ctx ArbbF32
     t    <- getDenseType ctx st 1 
     fnt <- getFunctionType ctx [t] [t] 
     myfun <- beginFunction ctx fnt "sum" 0
     a     <- getParameter myfun 0 0 
     b     <- getParameter myfun 1 0
   
     opDynamic myfun ArbbOpAddReduce [b] [a]
     endFunction myfun
     compile myfun
     binding <- getBindingNull 
     -- This part gets messy! 
     -- TODO: Clean up! 
     withArray [0..1023 :: Float] $ \ i1 -> 
      withArray [0 :: Float] $ \ o -> 
        do
         
          b1 <- createDenseBinding ctx (castPtr i1) 1 [1024] [4] 
          b2 <- createDenseBinding ctx (castPtr o) 1 [1] [4]
         
         
          g1 <- createGlobal ctx t "in1" b1;
          g2 <- createGlobal ctx t "out" b2;  
         
          v1 <- variableFromGlobal ctx g1;
          v2 <- variableFromGlobal ctx g2;       
          
          --r  <- createGlobal ctx t "result" binding
          --v2 <- variableFromGlobal ctx r 
          
          execute myfun [v2] [v1]
          str <- serializeFunction myfun 
          putStrLn (getCString str)
        
          -- access result
          result <- peekArray 1 (castPtr o :: Ptr Float) 
          putStrLn $ show $ result
          
         