

import ArbbVM 

{-
main = do 
     ctx <- defaultContext
     t   <- getScalarType ctx ArbbF32
     fun3 ctx t 
-}

main = do 
     ctx <- defaultContext
     t   <- getScalarType ctx ArbbF32 
     fnt <- getBinFunctionType ctx  t t t
     fun4 ctx t fnt
