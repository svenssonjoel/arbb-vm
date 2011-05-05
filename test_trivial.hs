

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Foreign.Marshal.Array
import Foreign.Ptr 

import C2HS

main = runReproducer$
  do 
     ctx <- getDefaultContext 
     t   <- getScalarType ctx ArbbF32
     putStrLn "Done."
