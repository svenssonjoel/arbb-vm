{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import Data.Word


{- 
 
-}

main = arbbSession$ do 
     sty <- getScalarType_ ArbbI32
     dty <- getDenseType_ sty 1
     dty2 <- getDenseType_ sty 2 
     bt <- getScalarType_ ArbbBoolean

     size_t <- getScalarType_ ArbbUsize
     dsize_t <- getDenseType_ size_t 1

     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [i1,i2] -> do 
        op_ ArbbOpAdd [out] [i1,i2] 
             
     reduceStep <- funDef_ "rS" [dty] [dty,size_t] $ \ [out] [inp,n] -> do 
        
        parts    <- createLocal_ dty2 "halves"
        h1       <- createLocal_ dty "h1"
        h2       <- createLocal_ dty "h2" 
        newArr   <- createLocal_ dty "new!"           
        midpoint <- createLocal_ size_t "middle"         

        zero <- usize_ 0 
        one  <- usize_ 1
        two  <- usize_ 2
                      
        op_ ArbbOpDiv [midpoint] [n,two]
        
        opDynamic_ ArbbOpSetRegularNesting [parts] [inp,two, midpoint]
        op_ ArbbOpExtractRow [h1] [parts,zero] 
        op_ ArbbOpExtractRow [h2] [parts,one]
        
        -- elementwise application of fun (zipWith) 
        map_ add [newArr] [h1,h2]   

        op_ ArbbOpCopy [out] [newArr] 
             
     reduce <- funDef_ "red" [sty] [dty,size_t] $ \ [out] [inp,n] -> do
       c   <- createLocal_ bt "cond" 
       one <- usize_ 1
       zero <- usize_ 0 
       two  <- usize_ 2
      
       currs <- createLocal_ size_t "currs"
       arr   <- createLocal_ dty "data"
      
       op_ ArbbOpCopy [currs] [n]     
       op_ ArbbOpCopy [arr] [inp]    
       while_ 
         (do
           op_ ArbbOpGreater [c] [currs,one]   
           return c
         ) 
         (do 
            call_ reduceStep [arr] [arr,currs] 
            op_ ArbbOpDiv [currs] [currs,two] 
         ) 
       opDynamic_ ArbbOpExtract [out] [arr,zero] 
        
     liftIO$ putStrLn "Done compiling function, now executing..."
 
   
     withArray_  (replicate (2^24) 1 ::[ Word32]) $ \ inp -> 
      withArray_ (replicate 8 0 :: [Word32]) $ \ out -> 
       do
       
        inb <- createDenseBinding_ (castPtr inp) 1 [2^24] [4]
        outb <- createDenseBinding_ (castPtr out) 1 [8] [4]
       
        gin <- createGlobal_ dty "input" inb
        gout <- createGlobal_ dty "output" outb
       
        vin <- variableFromGlobal_ gin
        vout <- variableFromGlobal_ gout
       
        n <- usize_ (2^24)
        binding <- getBindingNull_
        g       <- createGlobal_ sty "res" binding
        y       <- variableFromGlobal_ g
        --execute_ reduceStep [vout] [vin,n]     
        execute_ reduce [y] [vin,n] 
        
        
        result :: Word32 <- readScalar_ y      
         
        --result <- liftIO $ peekArray 8 (castPtr out :: Ptr Word32) 
        liftIO$ putStrLn $ show result
         
        
     