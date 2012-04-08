{-# LANGUAGE ScopedTypeVariables #-}
import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Foreign.Marshal.Array
import Foreign.Ptr 


import Data.Int

-----------------------------------------------------------------------------
-- Main


gen_fun = do 
  sty <- getScalarType_ ArbbI32
  dty <- getDenseType_  sty 1 
  
  
  fun <- funDef_ "myFun" [dty,sty] [dty] $ \ [o1,o2] [in1] -> do 
      bt <- getScalarType_ ArbbBoolean 
      stop <- int32_ 10  
      zero <- int32_ 0
      one  <- int32_ 1 

      i <- createLocal_ sty "loopcounter" 
      copy_ i zero -- init

      tmp <- createLocal_ dty "data" 
      copy_ tmp in1 
 
      while_ 
          ( do 
             t <- createLocal_ bt "cond" 
             op_ ArbbOpLess [t] [i,stop] 
             return t
          ) 
          (
            do 
             -- TODO: uncommenting the line below couses trouble! 
             -- op_ ArbbOpAdd [tmp] [tmp,in1]
             op_ ArbbOpAdd [i] [i,one]          
          ) 
      
      --j <- createLocal_ sty "hej" 
      --op_ ArbbOpAdd [j] [i,one]


      copy_ o1 tmp
      copy_ o2 i       

  return fun 

main = arbbSession$ do 
  
  sty <- getScalarType_ ArbbI32
  dty <- getDenseType_ sty 1 
  
  f <- gen_fun 
  
  withArray_ (replicate 10 1 :: [Int32]) $ \ in1 -> 
   withArray_ (replicate 10 0 :: [Int32]) $ \ out -> 
    do
     inb1 <- createDenseBinding_ (castPtr in1) 1 [10] [4] 
     gin1 <- createGlobal_ dty "gin1" inb1
     vin1 <- variableFromGlobal_ gin1
     
     outb1 <- createDenseBinding_ (castPtr out) 1 [10] [4] 
     gout1 <- createGlobal_ dty "gin1" outb1
     vout1 <- variableFromGlobal_ gout1

 
     --g    <- createGlobal_nobind_ dty "res" --outb
     --y    <- variableFromGlobal_ g 
        
     r  <- createGlobal_nobind_ sty "result" -- binding
     v  <- variableFromGlobal_ r  

     execute_ f [vout1,v] [vin1] 
     result :: Int32 <- readScalar_ v
     result2 <- liftIO $ peekArray 10 (castPtr out :: Ptr Int32)          

     liftIO$ putStrLn $ show result                
     liftIO$ putStrLn $ show result2                
       