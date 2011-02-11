{-# OPTIONS -XMultiParamTypeClasses 
            -XFlexibleContexts 
            -XFlexibleInstances #-}

import ArbbVM 

import Foreign.Marshal.Array
import Foreign.Ptr 

import C2HS
 
main = withDefaultContext $ \ctx -> 
      do 
       t   <- getScalarType ctx ArbbF32
       fnt <- getFunctionType ctx [t] [t,t,t] 
       myfun <- beginFunction ctx fnt "add" 
       a     <- getParameter myfun 0 0 
       b     <- getParameter myfun 0 1
       c     <- getParameter myfun 0 2
       d     <- getParameter myfun 1 0 
       op myfun ArbbOpAdd [d] [a,b]
       op myfun ArbbOpMul [d] [c,d]
       endFunction myfun
       compile myfun
       binding <- getNullBinding 
       -- This part gets messy! 
       -- TODO: Clean up! 
--       arr :: ForeignPtr __ <- newArray [10.0, 20.0, 30.0 :: Float]

       withArray [10.0, 20.0, 30.0 :: Float] $ \ input -> 
         do 
          g1 <- createConstant ctx t (castPtr input)
          g2 <- createConstant ctx t (plusPtr (castPtr input) 4) 
          g3 <- createConstant ctx t (plusPtr (castPtr input) 8)
          v1 <- variableFromGlobal ctx g1;
          v2 <- variableFromGlobal ctx g2;
          v3 <- variableFromGlobal ctx g3;
          r  <- createGlobal ctx t "result" binding
          v4 <- variableFromGlobal ctx r 

          v5 <- variableFromGlobal ctx r 

          -- view1 <- variableFromGlobal ctx g1;
          -- view2 <- variableFromGlobal ctx g1;
	  -- mutate view2, does view1 change?

          result0 <- readScalarFloat ctx v5
          putStrLn$ "Value of v5 before function call:"
          putStrLn (show result0)

          execute myfun [v4] [v1,v2,v3]
          -- TODO: Figure out how to best access results (of various types) 
          result <- readScalarFloat ctx v4

          putStrLn$ "First printing result of function call:"
          putStrLn (show result) 

          result2 <- readScalarFloat ctx v5
          putStrLn$ "Test: reading v5 which was built from same createGlobal"
          putStrLn (show result2)

          putStrLn "\nNow a ArBB-serialized function:"
          ser <- serializeFunction myfun;
          putStrLn ser
      