

import Intel.ArbbVM 

import Foreign.Marshal.Array
import Foreign.Ptr 

import C2HS

readScalarOfSize n ctx v = 
    allocaBytes n $ \ptr -> 
       do       
        readScalar ctx v ptr 
        peek (castPtr ptr)

-----------------------------------------------------------------------------
-- ifThenElse  
ifThenElse f c t e =
  do
   ifBranch f c      
   t -- op myfun ArbbOpSub [c] [a,a]
   elseBranch f 
   e -- op myfun ArbbOpDiv [c] [a,a]
   endIf f

while ctx f cond body = 
   do 
     beginLoop f ArbbLoopWhile
     beginLoopBlock f ArbbLoopBlockCond
     cond
     beginLoopBlock f ArbbLoopBlockBody
     body 
     endLoop f     


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
     beginLoop myfun ArbbLoopWhile
     beginLoopBlock myfun ArbbLoopBlockCond
     bt <- getScalarType ctx ArbbBoolean         
     lc <- createLocal myfun bt "a"
     op myfun ArbbOpGreater [lc] [b,tmp]      
     loopCondition myfun lc 

     beginLoopBlock myfun ArbbLoopBlockBody
     --(op myfun ArbbOpAdd [tmp] [tmp,a]) 
     op myfun ArbbOpAdd [tmp] [a,tmp]
     endLoop myfun     

     op myfun ArbbOpCopy [c] [tmp]          
    
     endFunction myfun
     compile myfun
     binding <- getBindingNull 
     -- This part gets messy! 
     -- TODO: Clean up! 
     withArray [1, 100, 0 :: Int] $ \ input -> 
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

