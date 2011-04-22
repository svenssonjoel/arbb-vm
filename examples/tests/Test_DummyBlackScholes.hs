{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import Data.Word

import Data.Time

{- Dummy BlackScholes -} 

riskfree, volatility :: Float
riskfree   = 0.02
volatility = 0.30


main = arbbSession$ do 
  sty <- getScalarType_ ArbbF32
  dty <- getDenseType_ sty 1
  bty <- getScalarType_ ArbbBoolean

  poly <- funDefCallable_ "poly" [sty] [sty] $ \ [o] [d] -> do 
    copy_ o d
    
  cnd <- funDefCallable_ "cnd" [sty] [sty] $ \ [o] [d] -> do 
    tmp <- createLocal_ sty "tmp" 
    call_ poly [tmp] [d]
    copy_ o tmp

  
  go' <- funDefCallable_ "go" [sty,sty] [sty,sty,sty] $ \ [o1,o2] [p,s,y] -> do 
                      
    cndD1 <- createLocal_ sty "cndD1" 
    cndD2 <- createLocal_ sty "cndD2" 
             

    a <- createLocal_ sty "a"
    b <- createLocal_ sty "b"

    copy_ a p
    copy_ b s
                      
    --call_ cnd [cndD1] [a] 
    --call_ cnd [cndD2] [b]      
    copy_ cndD1 a
    copy_ cndD2 b 
            
    copy_ o1 cndD1 -- tmp1
    copy_ o2 cndD2 -- tmp2

  go <- funDef_ "go" [sty,sty] [sty,sty,sty] $ \ [o1,o2] [p,s,y] -> do 
    call_ go' [o1,o2] [p,s,y]
  
  blackscholes' <- funDefCallable_ "blackscholes'" [dty,dty] [dty,dty,dty] $ \ [o1,o2] [i1,i2,i3] -> do
    map_ go' [o1,o2] [i1,i2,i3]                      

  blackscholes <- funDef_ "blackscholes" [dty,dty] [dty,dty,dty] $ \ [o1,o2] [i1,i2,i3] -> do
    call_ blackscholes' [o1,o2] [i1,i2,i3] 

  withArray_ [0..1000 :: Float] $ \ inp1 -> 
    withArray_ [0..1000 :: Float] $ \ inp2 -> 
      withArray_ [0..1000 :: Float] $ \ inp3 -> 
        withArray_ (replicate 1000 0 :: [Float]) $ \ out1 -> 
          withArray_ (replicate 1000 0 :: [Float]) $ \ out2 -> do

            -- inputs 
            inb1 <- createDenseBinding_ (castPtr inp1) 1 [1000] [4]                     
            inb2 <- createDenseBinding_ (castPtr inp2) 1 [1000] [4]                     
            inb3 <- createDenseBinding_ (castPtr inp3) 1 [1000] [4]                     

            gin1 <- createGlobal_ dty "input1" inb1
            gin2 <- createGlobal_ dty "input2" inb2
            gin3 <- createGlobal_ dty "input3" inb3

            vin1 <- variableFromGlobal_ gin1
            vin2 <- variableFromGlobal_ gin2
            vin3 <- variableFromGlobal_ gin3
                
                
            -- outputs 
            
            outb1 <- createDenseBinding_ (castPtr out1) 1 [1000] [4]
            outb2 <- createDenseBinding_ (castPtr out2) 1 [1000] [4] 
            
            gout1 <- createGlobal_ dty "output1" outb1
            gout2 <- createGlobal_ dty "output2" outb2 

            vout1 <- variableFromGlobal_ gout1
            vout2 <- variableFromGlobal_ gout2

            t1 <- liftIO getCurrentTime
            execute_ blackscholes [vout1,vout2] [vin1,vin2,vin3] 
            finish_ 
            t2 <- liftIO getCurrentTime


                     
            r1 <- liftIO$ peekArray 1000 out1
            r2 <- liftIO$ peekArray 1000 out2

            liftIO$ putStrLn$ show (zip r1 r2)
                     
   
            -- serialise and show
            str <- serializeFunction_ poly
            liftIO$ putStrLn (getCString str) 

            str <- serializeFunction_ cnd
            liftIO$ putStrLn (getCString str) 

            str <- serializeFunction_ go
            liftIO$ putStrLn (getCString str) 
        
            str <- serializeFunction_ blackscholes
            liftIO$ putStrLn (getCString str) 
            ---------------------

            liftIO$ putStrLn $ "time: " ++ ( show (diffUTCTime t2 t1) )  