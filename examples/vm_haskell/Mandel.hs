{-# LANGUAGE CPP, ScopedTypeVariables #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS

import Data.Complex
import Data.Int
import Data.Serialize
import Data.ByteString.Internal

-----------------------------------------------------------------------------

mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0 
  where   
   fn = magnitude
   loop i z 
    | i == max_depth = i
    | fn(z) >= 2.0   = i
    | otherwise      = loop (i+1) (z*z + c) 



mandelDef :: EmitArbb Function
mandelDef = do
  funDefS_ "mandel" [ArbbI32] [ArbbI32, ArbbF64] $ \ [out] [maxdepth, c] -> do
     
     [zer,one,max] <- mapM int32_ [0, 1, 100] -- Constants.
     twof <- float64_ 2.0

     counter <- local_int32_ "counter"
     op_ ArbbOpCopy [counter] [zer]
     while_ 
       (do
	  lc1 <- local_bool_ "loopcond1"
	  lc2 <- local_bool_ "loopcond2"
	  lc3 <- local_bool_ "loopcond3"
	  op_ ArbbOpLess   [lc1] [counter,max]
	  op_ ArbbOpLess   [lc2] [c,twof]
	  op_ ArbbOpLogAnd [lc3] [lc1,lc2]
	  return lc3
       )
       (do 
	   incr_int32_ counter 
	   return ()
       )

     op_ ArbbOpCopy [out] [counter]
     return ()
  
-----------------------------------------------------------------------------

main = arbbSession$ do 
  liftIO$ putStrLn$ "Starting..."

  mandel <- mandelDef

  res <- global_nobind_int32_ "res"
  liftMs (execute_ mandel [res]) 
	 [int32_ 100, float64_ 1.3]

  result :: Int32 <- readScalar_ res
  liftIO$ putStrLn$ "Result from function application: "++ show result
  

-----------------------------------------------------------------------------

main0 = do 

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

     x  <- newConstant ctx ity (100::Int32)
     y  <- newConstant ctx ity (30::Int32)
     quux <- createLocal myfun bty "quux"
     putStrLn "Before ArbbOpLess"
     op myfun ArbbOpLess [quux] [x,y]
     putStrLn "After ArbbOpLess"

     foo :: Int32 <- readScalarOfSize 4 ctx y
     putStrLn$ "Did a readScalarOfSize: "++ show foo

     zer     <- newConstant ctx ity (0::Int32)
     one     <- newConstant ctx ity (1::Int32)
     max     <- newConstant ctx ity (100::Int32)
     counter <- createLocal myfun ity "counter"
     op myfun ArbbOpCopy [counter] [zer]
     putStrLn "Counter initialized..."

     while myfun (do
         lc <- createLocal myfun bty "loopcond"
	 putStrLn "Inside while condition..."
         op myfun ArbbOpLess [lc] [counter,max]
	 putStrLn "Done with ArbbOpLess"
     	 return lc)
      (op myfun ArbbOpAdd [counter] [counter,one])

     putStrLn "Done emitting while"
     ------------------------------------------------------------

     op myfun ArbbOpMul [tmp] [a,b]       
     opDynamic myfun ArbbOpAddReduce [c] [tmp]

     putStrLn "At end of function"
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

-}