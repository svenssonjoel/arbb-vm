{-# LANGUAGE ScopedTypeVariables #-} 

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience


{-  
   BJS: Updated 1 Oct 2011 
        Now uses Convenience library
-}
-----------------------------------------------------------------------------
-- Main
main = arbbSession$ do 
     t   <- getScalarType_ ArbbF32
     bt  <- getScalarType_ ArbbBoolean 
     
     myfun <- funDef_ "condTest" [t] [t,t] $ \[out] [in1,in2] -> do 
       condition <- createLocal_ bt "cond" 
       op_ ArbbOpEqual [condition] [in1,in2] 
       if_ condition 
         (op_ ArbbOpSub [out] [in1,in1]) 
         (op_ ArbbOpDiv [out] [in1,in1])

     
   
     v1 <- float32_ 1.0 
     v2 <- float32_ 1.0 
     r  <- createGlobal_nobind_ t "result" 
     v3 <- variableFromGlobal_ r 
     execute_ myfun [v3] [v1,v2]
     -- TODO: Figure out how to best access results (of various types) 
     result :: Float <- readScalar_ v3 
     liftIO$ putStrLn$ show result
      