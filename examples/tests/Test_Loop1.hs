{-# LANGUAGE CPP #-}
import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Foreign.Marshal.Array
import Foreign.Ptr 

import C2HS

readScalarOfSize n ctx v = 
    allocaBytes n $ \ptr -> 
       do       
        readScalar ctx v ptr 
        peek (castPtr ptr)


-----------------------------------------------------------------------------
-- Main
main = do 
     ctx <- getDefaultContext 
     t   <- getScalarType ctx ArbbI32
     bt  <- getScalarType ctx ArbbBoolean 
     fnt <- getFunctionType ctx [t] [t,t] 
     myfun <- beginFunction ctx fnt "add" 0
     a     <- getParameter myfun 0 0 
     b     <- getParameter myfun 0 1
     c     <- getParameter myfun 1 0 

     tmp <- createLocal myfun t "tmp"
     op myfun ArbbOpCopy [tmp] [a]     

{-
     while ctx myfun 
       ( -- condition 
        do
         bt <- getScalarType ctx ArbbBoolean         
         lc <- createLocal myfun bt "loopcond"
         op myfun ArbbOpLess [lc] [tmp,b]      -- Loop on False
         loopCondition myfun lc 
       )
       -- body 
       (op myfun ArbbOpAdd [tmp] [tmp,a])      
-}
     
     -- TODO: This program fails on O3 !!! (works on O2)  
     -- TODO: Write the C version of this  and and see if it crashes too. 
     bt <- getScalarType ctx ArbbBoolean         
     lc <- createLocal myfun bt "condvar"
    

{-
 while (tmp < b) {
   tmp = tmp + 1;
 }
-}

#if 0 
     beginLoop myfun ArbbLoopWhile
     beginLoopBlock myfun ArbbLoopBlockCond
     op myfun ArbbOpLess [lc] [tmp,b]      
     loopCondition myfun lc -- exit loop if true (something seems wrong) 
   
     beginLoopBlock myfun ArbbLoopBlockBody
     --(op myfun ArbbOpAdd [tmp] [tmp,a]) 
     op myfun ArbbOpAdd [tmp] [a,tmp]
     endLoop myfun     
#else
     while myfun 
       ( -- condition 
        do
         bt <- getScalarType ctx ArbbBoolean         
         lc <- createLocal myfun bt "loopcond"
         op myfun ArbbOpLess [lc] [tmp,b]      -- Loop on False
	 return lc
       )
       -- body 
       (op myfun ArbbOpAdd [tmp] [tmp,a])      
#endif



     op myfun ArbbOpCopy [c] [tmp]          
    
     endFunction myfun
     compile myfun
     binding <- getBindingNull 

     str <- serializeFunction myfun 
     putStrLn "External representation of generated function:"
     putStrLn (getCString str)

     -- This part gets messy! 
     -- TODO: Clean up! 
     withArray [1, 100, 0 :: Word32] $ \ input -> 
        do 

          g1 <- createConstant ctx t (castPtr input)
          g2 <- createConstant ctx t (plusPtr (castPtr input) 4)   
          v1 <- variableFromGlobal ctx g1;
          v2 <- variableFromGlobal ctx g2;   
          r  <- createGlobal ctx t "result" binding
          v3 <- variableFromGlobal ctx r 
          execute myfun [v3] [v1,v2]
          -- TODO: Figure out how to best access results (of various types) 
          result <- readScalarOfSize 4 ctx v3 :: IO Int
          putStrLn (show result) 

