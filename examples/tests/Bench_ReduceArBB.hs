{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import Data.Word

import Data.Time
--import C2HS

{- 
   Update.. 
-}

main = arbbSession$ do 
     sty <- getScalarType_ ArbbI32
     dty <- getDenseType_ sty 1
                     
     reduce <- funDef_ "red" [sty] [dty] $ \ [out] [inp] -> do       
       opDynamic_ ArbbOpAddReduce [out] [inp] 
        
     liftIO$ putStrLn "Done compiling function, now executing..."
 
   
     withArray_  (replicate (2^24) 1 ::[ Word32]) $ \ inp -> 
--      withArray_ (replicate 8 0 :: [Word32]) $ \ out -> 
       do

        inb <- createDenseBinding_ (castPtr inp) 1 [2^24] [4]
 --       outb <- createDenseBinding_ (castPtr out) 1 [8] [4]
       
        gin <- createGlobal_ dty "input" inb
 --       gout <- createGlobal_ dty "output" outb
       
        vin <- variableFromGlobal_ gin
--        vout <- variableFromGlobal_ gout
       
        n <- usize_ (2^24)
        -- binding <- getBindingNull_
        g       <- createGlobal_nobind_ sty "res" -- binding
        y       <- variableFromGlobal_ g
        --execute_ reduceStep [vout] [vin,n]     
    
        t1 <- liftIO getCurrentTime                          
        execute_ reduce [y] [vin,n]
        finish_
        t2 <- liftIO getCurrentTime 

----------
        str <- serializeFunction_ reduce
        liftIO$ putStrLn (getCString str)
---------  


        result :: Word32 <- readScalar_ y      

        liftIO$ putStrLn $ "time: " ++ ( show (diffUTCTime t2 t1) )  
        --result <- liftIO $ peekArray 8 (castPtr out :: Ptr Word32) 
        liftIO$ putStrLn $ show result
         
        
     