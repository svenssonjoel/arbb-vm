


{- |  

   A module capturing common patterns in ArBB VM code emission and
   thereby making it easier to emit code.

 -}

module Intel.ArbbVM.Convenience 
 (
  ifThenElse,
  while,
  readScalarOfSize
 )
where

import Intel.ArbbVM 
import Foreign.Marshal.Array
import Foreign.Ptr 
import C2HS

-- ifThenElse  
ifThenElse f c t e =
  do
   ifBranch f c      
   t -- op myfun ArbbOpSub [c] [a,a]
   elseBranch f 
   e -- op myfun ArbbOpDiv [c] [a,a]
   endIf f


-- while loops
while :: Function -> (IO Variable) -> IO a1 -> IO ()
while f cond body = 
   do 
     beginLoop f ArbbLoopWhile
     beginLoopBlock f ArbbLoopBlockCond
     lc <- cond
     loopCondition f lc 

     beginLoopBlock f ArbbLoopBlockBody
     body 
     endLoop f     

-- fun Defs


-- global/constant shortcuts

readScalarOfSize :: Storable b => Int -> Context -> Variable -> IO b
readScalarOfSize n ctx v = 
    allocaBytes n $ \ptr -> 
       do       
        readScalar ctx v ptr 
        peek (castPtr ptr)

-- TODO: readScalar of storable

