

import Intel.ArbbVM 

import Foreign.Marshal.Array
import Foreign.Ptr 

import C2HS

readScalarOfSize n ctx v = 
    allocaBytes n $ \ptr -> 
       do       
        readScalar ctx v ptr 
        peek (castPtr ptr)

newConstantAlt :: Storable a => Context -> ScalarType -> a -> IO Variable 
newConstantAlt ctx st n = 
  do           
   t   <- getScalarType ctx st       
   tmp <- withArray [n] $ \x -> createConstant ctx t (castPtr x)
   variableFromGlobal ctx tmp
  

main = do 
     ctx <- getDefaultContext 
     t   <- getScalarType ctx ArbbF32
     fnt <- getFunctionType ctx [t] [t,t] 
     myfun <- beginFunction ctx fnt "add" 0
     a     <- getParameter myfun 0 0 
     b     <- getParameter myfun 0 1
     c     <- getParameter myfun 1 0 
     
     -- tmp <- newConstant ctx t (1.0 :: Float) 
     tmp <- newConstantAlt ctx ArbbF32 (1.0 :: Float)
     -- tmp2 <- newConstantAlt ctx ArbbF32 (1.0 :: Float)
     
     -- These two will differ! 
     --putStrLn $ show $ castPtr $ fromVariable tmp
     --putStrLn $ show $ castPtr $ fromVariable tmp2 
     
     op myfun ArbbOpAdd [c] [a,tmp]
     op myfun ArbbOpMul [c] [c,b]
     endFunction myfun
     --compile myfun
     -- binding <- getBindingNull 
     -- This part gets messy! 
     -- TODO: Clean up! 
     withArray [10.0, 2.0 :: Float] $ \ input -> 
        do 
          g1 <- createConstant ctx t (castPtr input)
          g2 <- createConstant ctx t (plusPtr (castPtr input) 4) 
         
          v1 <- variableFromGlobal ctx g1;
          v2 <- variableFromGlobal ctx g2;
         
          r  <- createGlobalNB ctx t "result" --binding
          v3 <- variableFromGlobal ctx r 
          execute myfun [v3] [v1,v2]
          -- TODO: Figure out how to best access results (of various types) 
          result <- readScalarOfSize 4 ctx v3 :: IO Float
          putStrLn (show result) 
      