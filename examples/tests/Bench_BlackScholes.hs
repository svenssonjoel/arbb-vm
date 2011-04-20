{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import Data.Word

import Data.Time

{- BlackScholes benchmark directly in the ArBB Bindings -} 

riskfree, volatility :: Float
riskfree   = 0.02
volatility = 0.30


main = arbbSession$ do 
  sty <- getScalarType_ ArbbF32
  dty <- getDenseType_ sty 1
  bty <- getScalarType_ ArbbBoolean

  -- Adoptation of (poly) from Accelerate example BlackScholes 
  -- This one takes "d" as input and computes the 
  -- 1.0 / (1.0 + 0.2316419 * abs d) internally 
  poly <- funDefCallable_ "poly" [sty] [sty] $ \ [o] [d] -> do 
    accm   <- createLocal_ sty "accumulator" 
    k      <- createLocal_ sty "k" 
    poly_k <- createLocal_ sty "poly_k" 
    abs_d  <- createLocal_ sty "abs_d" 
    tmp    <- createLocal_ sty "tmp"

    -- Constants 
    zero <- float32_ 0.0
    one  <- float32_ 1.0
    c1   <- float32_ 1.2316419
         
    -- coefficients
    coeff0 <- float32_ 0.0
    coeff1 <- float32_ 0.31938153
    coeff2 <- float32_ (-0.356563782)
    coeff3 <- float32_ 1.781477937 
    coeff4 <- float32_ (-1.821255978)
    coeff5 <- float32_ 1.330274429
    -- 

    -- calculate k 
    op_ ArbbOpAbs [abs_d] [d]
    op_ ArbbOpMul [k]     [c1,abs_d] 
    op_ ArbbOpDiv [k]     [one,k]

    -- calculate poly_k  (This part is called "horner" in the Accelerate example) 
    copy_ accm zero

    op_ ArbbOpMul [tmp] [k,coeff0] -- this is zero ? (am I wrong or should it optimize away)
    op_ ArbbOpAdd [accm] [tmp,accm] 

    op_ ArbbOpMul [tmp] [k,coeff1] 
    op_ ArbbOpAdd [accm] [tmp,accm]

    op_ ArbbOpMul [tmp] [k,coeff2] 
    op_ ArbbOpAdd [accm] [tmp,accm]

    op_ ArbbOpMul [tmp] [k,coeff3] 
    op_ ArbbOpAdd [accm] [tmp,accm]

    op_ ArbbOpMul [tmp] [k,coeff4] 
    op_ ArbbOpAdd [accm] [tmp,accm]
    
    op_ ArbbOpMul [tmp] [k,coeff5] 
    op_ ArbbOpAdd [accm] [tmp,accm]

    copy_ poly_k accm
             
    -- return result
    copy_ o poly_k
    
  -- FUNCTION cnd
  cnd <- funDefCallable_ "cnd" [sty] [sty] $ \ [o] [d] -> do 
    copy_ o d                        
    {-                       
    b <- createLocal_ bty "condition" 
                  
    rsqrt2pi <- float32_ 0.39894228040143267793994605993438  -- is that not too exact for a F32 ?
    negOFive <- float32_ (-0.5) 

    tmp <- createLocal_ sty "tmp" 
    dd  <- createLocal_ sty "dd" 
    edd <- createLocal_ sty "exp_dd" 
    
    -- Constants 
    zero <- float32_ 0.0
    one  <- float32_ 1.0
           
    -- 
    call_ poly [tmp] [d] 
    
    -- Compute edd
    op_ ArbbOpMul [dd] [d,d]
    op_ ArbbOpMul [dd] [negOFive,dd]
    op_ ArbbOpExp [edd] [dd]
    
    op_ ArbbOpMul [edd] [edd,tmp] -- mul by poly k
   

    op_ ArbbOpMul [tmp] [rsqrt2pi,edd]
   
     
    -- Conditional part 
    op_ ArbbOpGreater  [b] [d,zero] 
    if_ b 
        ( op_ ArbbOpSub [tmp] [one,tmp] )
        ( return () )
                 
        
    --result
    copy_ o tmp
    -}
    
  
  go <- funDefCallable_ "go" [sty,sty] [sty,sty,sty] $ \ [o1,o2] [p,s,y] -> do 
    tmp  <- createLocal_ sty "tmp"                      
    tmp1 <- createLocal_ sty "tmp1" 
    tmp2 <- createLocal_ sty "tmp2" 

    negrY  <- createLocal_ sty "negrY"    
    enegrY <- createLocal_ sty "enegrY"
    sqrtY  <- createLocal_ sty "sqrtY"
    vsqrtY <- createLocal_ sty "vsqrtY"
    pdivs  <- createLocal_ sty "p_div_s"
    lpdivs <- createLocal_ sty "log_p_div_s"
    
    d1    <- createLocal_ sty "d1"
    d2    <- createLocal_ sty "d2" 
    cndD1 <- createLocal_ sty "cndD1"
    cndD2 <- createLocal_ sty "cndD2" 

    r <- float32_ riskfree
    nr <- float32_ (-riskfree)
    v <- float32_ volatility
    c1 <- float32_ (riskfree + 0.5 * volatility * volatility) 

    -- Compute sqrtY and vsqrtY
    op_ ArbbOpSqrt [sqrtY] [y] 
    op_ ArbbOpMul [vsqrtY] [v,sqrtY]
    
    -- Compute pdivs and lpdivs
    op_ ArbbOpDiv [pdivs] [p,s]
    op_ ArbbOpLn  [lpdivs] [pdivs]
    
    -- Compute negrY and enegrY
    op_ ArbbOpMul [negrY] [nr,y]
    op_ ArbbOpExp [enegrY] [negrY] 
    
    -- Compute d1 
    op_ ArbbOpAdd [tmp] [lpdivs,c1] 
    op_ ArbbOpMul [tmp] [tmp,y] 
    op_ ArbbOpDiv [d1]  [tmp,vsqrtY] 
        
    -- Compute d2 
    op_ ArbbOpSub [d2] [d1,vsqrtY] 
    
        
    -- cndD1 
    call_ cnd [cndD1] [d1] 
    -- copy_ cndD1 d1

    -- cndD2 
    call_ cnd [cndD2] [d2]      
    -- copy_ cndD2 d2

    --CONSTRUCTION AREA    
    -- to begin with 
    op_ ArbbOpSub [tmp1] [p,s] -- price * cndD1 - strike * expRT * cndD2
    op_ ArbbOpSub [tmp2] [s,p] -- strike * expRT * (1.0 - cndD2) - price * (1.0 - cndD1)
    ---
            
    copy_ o1 cndD1 -- tmp1
    copy_ o2 cndD2 -- tmp2
  
  blackscholes <- funDef_ "blackscholes" [dty,dty] [dty,dty,dty] $ \ [o1,o2] [i1,i2,i3] -> do
    map_ go [o1,o2] [i1,i2,i3]                      

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