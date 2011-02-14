
-- Verifies that multiple calls to default context return the SAME
-- address.

import Intel.ArbbVM 
import Foreign.Ptr 
import System.Exit

main = do 
     ctx1 <- getDefaultContext 
     ctx2 <- getDefaultContext 
     let n1 = ptrToIntPtr$ fromContext ctx1
	 n2 = ptrToIntPtr$ fromContext ctx2
     putStrLn$ "Address 1 "++ show n1
     putStrLn$ "Address 2 "++ show n2

     if n1 == n2 then 
       putStrLn$ "Addresses the same, as expected."
      else 
       exitFailure
