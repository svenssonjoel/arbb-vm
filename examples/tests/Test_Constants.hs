{-# LANGUAGE ScopedTypeVariables #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

{- BJS: updated 2 oct 2011
        Now uses Convenienve library 
-} 

----------------------------------------------------------------------------
-- constants declared inside a function and outside.
main = arbbSession$ do 
   
     t   <- getScalarType_ ArbbF32
   
     myfun <- funDef_ "add" [t] [t,t] $ \[out] [in1,in2] -> do 
       tmp <- float32_ 1.0 -- constant
     
       op_ ArbbOpAdd [out] [in1,tmp]
       op_ ArbbOpMul [out] [out,in2]
   
     v1 <- float32_ 1.0 --constant
     v2 <- float32_ 2.0 --constant
         
     r  <- createGlobal_nobind_ t "result" 
     v3 <- variableFromGlobal_ r 
     execute_ myfun [v3] [v1,v2]
        
     result :: Float <- readScalar_ v3 
     liftIO$ putStrLn (show result) 
      