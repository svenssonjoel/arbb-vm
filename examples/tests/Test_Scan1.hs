{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import Data.Word
import Data.Int


{- 
   Attempting Kogge-Stone Parallel prefix 
    (This awkward implementation requires a identity element 
     for the operation used) 
 
-}

main = arbbSession$ do 
     sty <- getScalarType_ ArbbI32
     dty <- getDenseType_ sty 1
     dty2 <- getDenseType_ sty 2 
     bt <- getScalarType_ ArbbBoolean

     size_t <- getScalarType_ ArbbUsize
     isize_t <- getScalarType_ ArbbIsize
     dsize_t <- getDenseType_ size_t 1
     disize_t <- getDenseType_ isize_t 1

     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [i1,i2] -> do 
        op_ ArbbOpAdd [out] [i1,i2] 
             
     scanStep <- funDef_ "st" [dty] [dty,sty] $ \ [out] [inp,n] -> do 
        
        minusone <- int32_ (-1)
        one <- int32_ 1
        two <- int32_ 2
        step <- int32_ 1
        idval <- int32_ 0
        -- tmp <- createLocal_ sty "castn"
        
        indices <- createLocal_ dsize_t "ixs"
        indices' <- createLocal_ dty "ixs_"
        start <- createLocal_ sty "step"
        length <- createLocal_ size_t "arrlen"
        tmpArr <- createLocal_ dty "tmparr"
        
        --start' <- createLocal_ isize_t "st_"
        -- step'  <- createLocal_ isize_t "s_"
           
        op_ ArbbOpLength [length] [inp]         
        op_ ArbbOpMul [start] [minusone,n] 
       
       
       
        --op_ ArbbOpCast [start'] [start] 
        --op_ ArbbOpCast [step'] [step]
        opDynamic_ ArbbOpIndex [indices'] [start, length, step]
        op_ ArbbOpBitwiseCast [indices] [indices']
        opDynamic_ ArbbOpGather [tmpArr] [inp,indices,idval] 
          
       
       
        --op_ ArbbOpCast [out] [indices] 
        op_ ArbbOpCopy [out] [tmpArr]           
  {-           
     scan <- funDef_ "scan" [sty] [dty,size_t] $ \ [out] [inp,n] -> do
       c   <- createLocal_ bt "cond" 
       one <- usize_ 1
       zero <- usize_ 0 
       two  <- usize_ 2
      
       currs <- createLocal_ size_t "currs"
       arr   <- createLocal_ dty "data"
      
       stage <- createLocal_ size_t "stage"
       op_ ArbbOpCopy [stage] [one]
      
       op_ ArbbOpCopy [currs] [n]     
       op_ ArbbOpCopy [arr] [inp]    
       while_ 
         (do
           op_ ArbbOpGreater [c] [currs,one]   
           return c
         ) 
         (do 
            call_ scanStep [arr] [arr,stage]
            op_ ArbbOpMul [stage] [stage,two] 
            op_ ArbbOpDiv [currs] [currs,two] 
         ) 
       op_ ArbbOpCopy [out] [arr] 
    -}    
     liftIO$ putStrLn "Done compiling function, now executing..."
 
   
     withArray_  ([1,2,3,4,5,6,7,8 :: Int32]) $ \ inp -> 
      withArray_ (replicate 8 0 :: [Int32]) $ \ out -> 
       do
       
        inb <- createDenseBinding_ (castPtr inp) 1 [8] [4]
        outb <- createDenseBinding_ (castPtr out) 1 [8] [4]
       
        gin <- createGlobal_ dty "input" inb
        gout <- createGlobal_ dty "output" outb
       
        vin <- variableFromGlobal_ gin
        vout <- variableFromGlobal_ gout
       
        n <- int32_ 2
        --binding <- getBindingNull_
        --g       <- createGlobal_ sty "res" binding
        --y       <- variableFromGlobal_ g
        --execute_ reduceStep [vout] [vin,n]     
        execute_ scanStep [vout] [vin,n] 
        
        
        --result :: Int32 <- readScalar_ y      
         
        result <- liftIO $ peekArray 8 (castPtr out :: Ptr Int32) 
        liftIO$ putStrLn $ show result
         
        
     