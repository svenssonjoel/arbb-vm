{-# LANGUAGE CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS

import Data.Int
import Data.Serialize
import Data.ByteString.Internal

readScalarOfSize n ctx v = 
    allocaBytes n $ \ptr -> 
       do       
        readScalar ctx v ptr 
        peek (castPtr ptr)

-----------------------------------------------------------------------------

-- withSerialized :: Serialize a => a -> (Ptr () -> b) -> b
withSerialized :: Serialize a => a -> (Ptr () -> IO b) -> IO b
withSerialized x fn =    
   withForeignPtr fptr (fn . castPtr)
 where 
   (fptr,_,_) = toForeignPtr (encode x)
  

-----------------------------------------------------------------------------
-- Main
main = do 

--     let size = 10 * 1024 * 1024 
     let size = 1024 

     ctx   <- getDefaultContext 
     bty   <- getScalarType ctx ArbbBoolean         
     sty   <- getScalarType ctx ArbbF32
     ity   <- getScalarType ctx ArbbI32
     arrty <- getDenseType ctx sty 1 
     fnt <- getFunctionType ctx [arrty] [arrty,arrty] 

     ------------------------------------------------------------
     myfun <- beginFunction ctx fnt "sum" 0
     a     <- getParameter myfun 0 0 -- max depth
     b     <- getParameter myfun 0 1 -- 
     c     <- getParameter myfun 1 0

     tmp <- createLocal myfun arrty "tmp" 

#if 1
     counter <- createLocal myfun ity "counter"
     bound  <- withSerialized (100::Int32) $ createConstant ctx ity 
     bound' <- variableFromGlobal ctx bound

     op myfun ArbbOpCopy [counter] [bound']
     while myfun (do
         bt <- getScalarType ctx ArbbBoolean         
         lc <- createLocal myfun bt "loopcond"
	 putStrLn "Inside while condition..."
-- HAVING TROUBLE WITH THIS LINE:
         op myfun ArbbOpLess [lc] [counter,b]      -- Loop on False
	 putStrLn "Done with ArbbOpLess"
--         op myfun ArbbOpLess [lc] [b,b]      -- Loop on False
     	 return lc)
--      (return ())
      (op myfun ArbbOpAdd [counter] [counter,a])

     -- beginLoop      myfun ArbbLoopWhile
     -- beginLoopBlock myfun ArbbLoopBlockCond
     -- bty <- getScalarType ctx ArbbBoolean         
     -- lc  <- createLocal myfun bty "loopcond"
     -- op myfun ArbbOpLess [lc] [counter,b]      -- Loop on False
     -- loopCondition myfun lc 
     -- beginLoopBlock myfun ArbbLoopBlockBody
     -- (op myfun ArbbOpAdd [counter] [counter,a])
     -- endLoop myfun     
#endif
     ------------------------------------------------------------

     op myfun ArbbOpMul [tmp] [a,b]       
     opDynamic myfun ArbbOpAddReduce [c] [tmp]

     endFunction myfun
     ------------------------------------------------------------

     putStrLn "Done streaming function AST."
     compile myfun
     putStrLn "Done compiling function."
     binding <- getBindingNull 
     -- This part gets messy! 
     -- TODO: Clean up! 
     withArray [1.0 :: Float | _ <- [0..size-1]] $ \ i1 -> 
      withArray [1.0 :: Float | _ <- [0..size-1]] $ \ i2 ->   
       withArray [0 :: Float] $ \ out -> 
        do
         
          b1 <- createDenseBinding ctx (castPtr i1) 1 [size] [4] 
          b2 <- createDenseBinding ctx (castPtr i2) 1 [size] [4]       
          b3 <- createDenseBinding ctx (castPtr out) 1 [1] [4]         
         
          g1 <- createGlobal ctx arrty "in1" b1
          g2 <- createGlobal ctx arrty "in2" b2
          g3 <- createGlobal ctx arrty "out" b3  
         
          v1 <- variableFromGlobal ctx g1
          v2 <- variableFromGlobal ctx g2
          v3 <- variableFromGlobal ctx g3      
          
          --r  <- createGlobal ctx t "result" binding
          --v2 <- variableFromGlobal ctx r 
          
          execute myfun [v3] [v1,v2]
          str <- serializeFunction myfun 
	  putStrLn "External representation of generated function:"
          putStrLn (getCString str)
        
          -- access result
          result <- peekArray 1 (castPtr out :: Ptr Float) 
          putStrLn $ show $ result
          
         

{-
     ctx <- getDefaultContext 
     t   <- getScalarType ctx ArbbI32
     bt  <- getScalarType ctx ArbbBoolean 
     fnt <- getFunctionType ctx [t] [t,t] 
     myfun <- beginFunction ctx fnt "add" 0
     a     <- getParameter myfun 0 0 
     b     <- getParameter myfun 0 1
     c     <- getParameter myfun 1 0 

     tmp <- createLocal myfun t "tmp"
     op myfun ArbbOpCopy [tmp] [a]     

     beginLoop      myfun ArbbLoopWhile
     beginLoopBlock myfun ArbbLoopBlockCond
     bt <- getScalarType ctx ArbbBoolean         
     lc <- createLocal myfun bt "loopcond"
     op myfun ArbbOpLess [lc] [tmp,b]      -- Loop on False
     loopCondition myfun lc 

     beginLoopBlock myfun ArbbLoopBlockBody
     (op myfun ArbbOpAdd [tmp] [tmp,a]) 
     endLoop myfun     

     op myfun ArbbOpCopy [c] [tmp]
    
     endFunction myfun
     putStrLn "Done streaming function AST."

     compile myfun
     putStrLn "Done compiling function."

     binding <- getBindingNull 
     -- This part gets messy! 
     -- TODO: Clean up! 
     withArray [1, 100, 0 :: Int] $ \ input -> 
        do 

          g1 <- createConstant ctx t (castPtr input)
          g2 <- createConstant ctx t (plusPtr (castPtr input) 4)   
          v1 <- variableFromGlobal ctx g1;
          v2 <- variableFromGlobal ctx g2;   
          r  <- createGlobal ctx t "result" binding
          v3 <- variableFromGlobal ctx r 
          execute myfun [v3] [v1,v2]
          -- TODO: Figure out how to best access results (of various types) 
          result <- readScalarOfSize 4 ctx v3 :: IO Int
          putStrLn (show result) 

-}