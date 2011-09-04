
-- -----------------------------------------------------------------------------
-- Test_DotProd

import Intel.ArbbVM 

import Foreign.Marshal.Array
import Foreign.Ptr 

import C2HS

len = 2^22-1

main = do 
     ctx <- getDefaultContext 
     st   <- getScalarType ctx ArbbF32
     t    <- getDenseType ctx st 1 
     fnt <- getFunctionType ctx [t] [t,t] 
     myfun <- beginFunction ctx fnt "sum" 0
     a     <- getParameter myfun 0 0
     b     <- getParameter myfun 0 1 
     c     <- getParameter myfun 1 0

     tmp <- createLocal myfun t "tmp" 
     
     op myfun ArbbOpMul [tmp] [a,b]  
     
     opDynamic myfun ArbbOpAddReduce [c] [tmp]
     endFunction myfun
     --compile myfun
     -- binding <- getBindingNull 
     -- This part gets messy! 
     -- TODO: Clean up! 
     withArray [1.0 :: Float | _ <- [0..len]] $ \ i1 -> 
      withArray [1.0 :: Float | _ <- [0..len]] $ \ i2 ->   
       withArray [0 :: Float] $ \ o -> 
        do
         
          b1 <- createDenseBinding ctx (castPtr i1) 1 [len] [4] 
          b2 <- createDenseBinding ctx (castPtr i2) 1 [len] [4]       
          b3 <- createDenseBinding ctx (castPtr o) 1 [1] [4]
         
         
          g1 <- createGlobal ctx t "in1" b1
          g2 <- createGlobal ctx t "in2" b2
          g3 <- createGlobal ctx t "out" b3  
         
          v1 <- variableFromGlobal ctx g1
          v2 <- variableFromGlobal ctx g2
          v3 <- variableFromGlobal ctx g3      
          
          --r  <- createGlobal ctx t "result" binding
          --v2 <- variableFromGlobal ctx r 
          
          execute myfun [v3] [v1,v2]
          str <- serializeFunction myfun 
          putStrLn (getCString str)
        
          -- access result
          result <- peekArray 1 (castPtr o :: Ptr Float) 
          putStrLn $ show $ result
          
         