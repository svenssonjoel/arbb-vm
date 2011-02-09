

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


-- TODO: Does not work
main = do 
     ctx <- defaultContext
     t   <- getScalarType ctx ArbbF32 
     fnt <- withArray [TypeStruct t,TypeStruct t] $ \inp -> 
              withArray [TypeStruct t] $ \outp -> getFunctionType_ ctx  outp inp 1 2
     fun4 ctx t fnt
