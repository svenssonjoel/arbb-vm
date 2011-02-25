{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import Data.Word
--import C2HS

{- 
   Update.. 
-}

main = arbbSession$ do 
     sty <- getScalarType_ ArbbI32
     dty <- getDenseType_ sty 1
     size_t <- getScalarType_ ArbbUsize
     dsize_t <- getDenseType_ size_t 1

     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [i1,i2] -> do 
        op_ ArbbOpAdd [out] [i1,i2] 
      
     arraydistr <- funDef_ "apa" [dty] [dty] $ \ [out] [inp] -> do 
        
        
        newArr  <- createLocal_ dty "new!"           
        arr     <- createLocal_ dty "quack"
        two'    <- createLocal_ size_t "GRRR"
        
        one <- int32_ 1
        two <- usize_ 2
        twenty <- usize_ 20        
        
        op_ ArbbOpCopy [two'] [two]            

        op_ ArbbOpCopy [arr] [inp]
           
        --opDynamic_ ArbbOpNewVector [newArr] [twenty] 
          
        op_ ArbbOpRepeat  [newArr] [arr]    -- REPEAT seems to be broken (doesnt work for any kind of inputs) 
        op_ ArbbOpReverse [newArr] [newArr] 
        
        op_ ArbbOpCopy [out] [newArr] 
        
        
     liftIO$ putStrLn "Done compiling function, now executing..."
 
   
     withArray_ [1..10 ::Word32] $ \ inp -> 
      withArray_ (replicate 20 0 :: [Word32]) $ \ out -> 
       do
       
        inb <- createDenseBinding_ (castPtr inp) 1 [10] [4]
        outb <- createDenseBinding_ (castPtr out) 1 [20] [4]
       
        gin <- createGlobal_ dty "input" inb
        gout <- createGlobal_ dty "output" outb
       
        vin <- variableFromGlobal_ gin
        vout <- variableFromGlobal_ gout
      
        execute_ arraydistr [vout] [vin]     
         
         
        result <- liftIO $ peekArray 20 (castPtr out :: Ptr Word32) 
        liftIO$ putStrLn $ show result
         
        
     