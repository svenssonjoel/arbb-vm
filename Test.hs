

import ArbbVM 

import Foreign.Marshal.Array
import Foreign.Ptr 

import C2HS

main = do 
     ctx <- defaultContext 
     t   <- getScalarType ctx ArbbF32
     fnt <- getFunctionType ctx [t] [t,t] 
     myfun <- beginFunction ctx fnt "add" 
     a     <- getParameter myfun 0 0 
     b     <- getParameter myfun 0 1
     c     <- getParameter myfun 1 0 
     op myfun ArbbOpAdd [c] [a,b]
     endFunction myfun
     compile myfun
     binding <- getNullBinding 
     -- This part gets messy! 
     -- TODO: Clean up! 
     withArray [10.0, 20.0,0.0 :: Float] $ \ input -> 
        do 
          g1 <- createConstant ctx t (castPtr input)
          g2 <- createConstant ctx t (plusPtr (castPtr input) 4) 
          v1 <- variableFromGlobal ctx g1;
          v2 <- variableFromGlobal ctx g2;
          r  <- createGlobal ctx t "result" binding
          v3 <- variableFromGlobal ctx r 
          execute myfun [v3] [v1,v2]
          -- TODO: Figure out how to best access results (of various types) 
          result <- readScalarFloat ctx v3
          putStrLn (show result) 
         
