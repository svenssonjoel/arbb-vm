
-- -----------------------------------------------------------------------------
-- Test_Simple2.hs 
-- zipWith + on two  arrays of length 1024 

import Intel.ArbbVM 

import Foreign.Marshal.Array
import Foreign.Ptr 

import C2HS

main = do 
     ctx <- getDefaultContext 
     st   <- getScalarType ctx ArbbF32
     t    <- getDenseType ctx st 1 
     fnt <- getFunctionType ctx [t] [t,t] 
     myfun <- beginFunction ctx fnt "add" 0
     a     <- getParameter myfun 0 0 
     b     <- getParameter myfun 0 1
     c     <- getParameter myfun 1 0
   
     op myfun ArbbOpAdd [c] [a,b]
     endFunction myfun
     compile myfun
     binding <- getBindingNull 
     -- This part gets messy! 
     -- TODO: Clean up! 
     withArray [0..1023 :: Float] $ \ i1 -> 
      withArray [0..1023 :: Float] $ \ i2 -> 
       withArray [0..1023 :: Float] $ \ o  -> 
        do
         
          b1 <- createDenseBinding ctx (castPtr i1) 1 [1024] [4] 
          b2 <- createDenseBinding ctx (castPtr i2) 1 [1024] [4]  
          b3 <- createDenseBinding ctx (castPtr o) 1 [1024] [4]
         
          g1 <- createGlobal ctx t "in1" b1;
          g2 <- createGlobal ctx t "in2" b2;
          g3 <- createGlobal ctx t "out" b3;
          
          v1 <- variableFromGlobal ctx g1;
          v2 <- variableFromGlobal ctx g2;   
          v3 <- variableFromGlobal ctx g3;
          
          execute myfun [v3] [v1,v2]
          -- TODO: Figure out how to best access results (of various types) 
          -- result <- readScalarOfSize 4 ctx v3 :: IO Float 
          -- putStrLn (show result)
          result <- peekArray 1024 (castPtr o :: Ptr Float) 
          putStrLn $ show $ result
         