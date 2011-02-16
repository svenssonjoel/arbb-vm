

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



-----------------------------------------------------------------------------
-- Main
main = do 
     ctx <- getDefaultContext 
     t   <- getScalarType ctx ArbbF32
     bt  <- getScalarType ctx ArbbBoolean 
     fnt <- getFunctionType ctx [t] [t,t] 
     myfun <- beginFunction ctx fnt "add" 0
     a     <- getParameter myfun 0 0 
     b     <- getParameter myfun 0 1
     c     <- getParameter myfun 1 0 

     condition <- createLocal myfun bt "cond"
          
     op myfun ArbbOpEqual [condition] [a,b]      
     

     ifThenElse myfun condition 
       (op myfun ArbbOpSub [c] [a,a])
       (op myfun ArbbOpDiv [c] [a,a])
    
     endFunction myfun
     compile myfun
     binding <- getBindingNull 
     -- This part gets messy! 
     -- TODO: Clean up! 
     withArray [10.0, 100.0,30.0 :: Float] $ \ input -> 
        do 
          g1 <- createConstant ctx t (castPtr input)
          g2 <- createConstant ctx t (plusPtr (castPtr input) 4)   
          v1 <- variableFromGlobal ctx g1;
          v2 <- variableFromGlobal ctx g2;   
          r  <- createGlobal ctx t "result" binding
          v3 <- variableFromGlobal ctx r 
          execute myfun [v3] [v1,v2]
          -- TODO: Figure out how to best access results (of various types) 
          result <- readScalarOfSize 4 ctx v3 :: IO Float
          putStrLn (show result) 
      