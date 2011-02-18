{-# LANGUAGE CPP, ScopedTypeVariables #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS


main = arbbSession$ do 
     sty   <- getScalarType_  ArbbI32
     dty   <- getDenseType_ sty 1 

     reduce <- funDef_ "reduceAdd" [dty] [dty] $ \ [out] [inp] -> do
        opDynamic_ ArbbOpAddReduce  [out] [inp]

     liftIO$ putStrLn "Done compiling function, now executing..."

     i_data <- liftIO$ newArray (replicate 1024 1 :: [Word32])
     o_data <- liftIO$ newArray [0 :: Word32]
     i_array <- createDenseBinding_ (castPtr i_data) 1 [1024] [4]
     o_array <- createDenseBinding_ (castPtr o_data) 1 [1] [4] 
     
     g_in  <- createGlobal_  dty "in" i_array;       
     g_out <- createGlobal_  dty "res" o_array;
  
     v1 <- variableFromGlobal_ g_in;
     v2 <- variableFromGlobal_ g_out;       
         
     execute_ reduce [v2] [v1]
     result <- liftIO$ peekArray 1 (castPtr o_data :: Ptr Word32)    

     liftIO$ putStrLn$ "Result from function application: "++ show result
     
     