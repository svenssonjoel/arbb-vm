


{- |  

   A module capturing common patterns in ArBB VM code emission and
   thereby making it easier to emit code.

 -}

module Intel.ArbbVM.Convenience 
 (
  ifThenElse
 )
where

import Intel.ArbbVM 

-- ifThenElse  
ifThenElse f c t e =
  do
   ifBranch f c      
   t -- op myfun ArbbOpSub [c] [a,a]
   elseBranch f 
   e -- op myfun ArbbOpDiv [c] [a,a]
   endIf f


-- while loops
