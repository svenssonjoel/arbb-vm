
-- -----------------------------------------------------------------------------
-- Map increment over an array   

import Intel.ArbbVM 

import Foreign.Marshal.Array
import Foreign.Ptr 

import C2HS

newConstant :: Storable a => Context -> Type -> a -> IO Variable 
newConstant ctx t n = 
  do           
   tmp <- withArray [n] $ \x -> createConstant ctx t (castPtr x)
   variableFromGlobal ctx tmp


main = do 
     ctx <- getDefaultContext 
     st   <- getScalarType ctx ArbbF32
     t    <- getDenseType ctx st 1 
    

-- myfun "increment" ( To be mapped ) 
     fnt <- getFunctionType ctx [st] [st] 
     myfun <- beginFunction ctx fnt "increment" 0
     a     <- getParameter myfun 0 0 
     b     <- getParameter myfun 1 0
   
     one <- newConstant ctx st (1.0 :: Float)      
        
     op myfun ArbbOpAdd [b] [a,one]
     endFunction myfun
--     compile myfun
-- myfun ends
     
-- Caller starts 
     caller_t  <- getFunctionType ctx [t] [t]      
     caller    <- beginFunction ctx caller_t "caller" 0
     in_array  <- getParameter caller 0 0 
     out_array <- getParameter caller 1 0
     
     -- Map myfun over an array 
     callOp caller ArbbOpMap myfun [out_array] [in_array]
   
     endFunction caller
  --   compile caller
-- Caller ends
 

     binding <- getBindingNull 
     -- This part gets messy! 
     -- TODO: Clean up! 
     withArray [0..1023 :: Float] $ \ i1 -> 
       withArray [0..1023 :: Float] $ \ o  -> 
        do
         
          b1 <- createDenseBinding ctx (castPtr i1) 1 [1024] [4] 
          b2 <- createDenseBinding ctx (castPtr o) 1 [1024] [4]
         
          g1 <- createGlobal ctx t "in1" b1
          g2 <- createGlobal ctx t "out" b2
          
          v1 <- variableFromGlobal ctx g1
          v2 <- variableFromGlobal ctx g2
          
          execute caller [v2] [v1]
          str <- serializeFunction myfun 
          putStrLn (getCString str)
          -- TODO: Figure out how to best access results (of various types) 
          -- result <- readScalarOfSize 4 ctx v3 :: IO Float 
          -- putStrLn (show result)
          result <- peekArray 1024 (castPtr o :: Ptr Float) 
          putStrLn $ show $ result
         