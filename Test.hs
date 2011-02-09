

import ArbbVM 

import Foreign.Marshal.Array


{-
main = do 
     ctx <- defaultContext
     t   <- getScalarType ctx ArbbF32
     fun3 ctx t 
-}

{- 
main = do 
     ctx <- defaultContext
     t   <- getScalarType ctx ArbbF32 
     fnt <- getBinFunctionType ctx  t t t
     fun4 ctx t fnt
-}

{-
main = do 
     ctx <- defaultContext
     t   <- getScalarType ctx ArbbF32 
     fnt <- withArray [t,t] $ \inp -> 
             withArray [t] $ \oupt -> getFunctionType ctx  outp inp 1 2
     fun4 ctx t fnt
-}

main = do 
     ctx <- defaultContext
     t   <- getScalarType ctx ArbbF32 
     fnt <- getFunctionType ctx  [t] [t,t] 1 2
     fun4 ctx t fnt
