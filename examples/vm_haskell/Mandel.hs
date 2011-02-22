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
import System.Environment

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
     
     [zer,one,max] <- mapM int32_   [0, 1, 100] -- Constants.
     [zerf, twof]  <- mapM float64_ [0.0, 2.0]

     counter <- local_int32_ "counter"
     z       <- local_float64_ "z"
     copy_ counter zer
     copy_ z zerf

     while_ 
       (do
	  lc1 <- local_bool_ "bool1"
	  lc2 <- local_bool_ "bool2"
	  lc3 <- local_bool_ "bool3"
	  op_ ArbbOpLess   [lc1] [counter,max]
	  op_ ArbbOpLess   [lc2] [z, twof]
	  op_ ArbbOpLogAnd [lc3] [lc1,lc2]
	  return lc3
       )
       (do 
	   incr_int32_ counter 
           op_ ArbbOpMul [z] [z,z];
           op_ ArbbOpAdd [z] [z,c];
	   return ()
       )

     op_ ArbbOpCopy [out] [counter]
     return ()

-----------------------------------------------------------------------------

--_ = withArray + 3

runMandel :: (Int, Int, Int) -> EmitArbb ()
runMandel (max_row, max_col, max_depth) = 
 do 
    mandel <- mandelDef
    sty    <- getScalarType_ ArbbI32
    arrty  <- getDenseType_ sty 1 

    glob <- global_nobind_ arrty "array"
-- dense<any,1> = index(any start, $usize length, any stride);
-- dense<any,2> = index(any start, $usize length, any stride, $usize num_times, $boolean
-- along_row);
-- Cannot execute opcode except inside a function:
--    c64 <- int32_ 64
--    op_ ArbbOpNewVector [glob] [c64]
    print_ "Made global vector variable"

    testfun <- funDefS_ "testfun" [ArbbI32] [ArbbI32] $ \ [out] [inp] -> do
      print_ "gen code for simple test"
      arr <- createLocal_ arrty "tmp"
--      op_ ArbbOpNewVector [arr] [c64]
      fn <- getFun ""
--      c64 <- createLocal_ sty 
      c64 <- int32_ 64
      opDynamic_ ArbbOpNewVector [arr] [c64]
      copy_ out inp


    str <- serializeFunction_ mandel
    print_ "Generated mandel kernel:"
    print_ (getCString str)

#if 0
    withArray_ [0..1023 :: Float] $ \ i1 ->        
      withArray_ [0..1023 :: Float] $ \ o  -> do
        print_$ "With array1 " ++ show (ptrToIntPtr i1) 
        print_$ "With array2 " ++ show (ptrToIntPtr o) 
         
        b1  <- createDenseBinding_ (castPtr i1) 1 [1024] [4] 
        b2  <- createDenseBinding_ (castPtr o)  1 [1024] [4]

        return ()
#endif
     --      g1 <- createGlobal ctx t "in1" b1
     --      g2 <- createGlobal ctx t "out" b2
     --      v1 <- variableFromGlobal ctx g1
     --      v2 <- variableFromGlobal ctx g2
          
     --      execute caller [v2] [v1]
     --      putStrLn (getCString str)
     --      result <- peekArray 1024 (castPtr o :: Ptr Float) 
     --      putStrLn $ show $ result


    res    <- global_nobind_int32_ "res"
    liftMs (execute_ mandel [res]) [int32_ 100, float64_ 0.33]
    result :: Int32 <- readScalar_ res

    liftIO$ putStrLn$ "Mandel checksum: "++ show result
         
    return ()


main = arbbSession$ do 

  args <- liftIO$ getArgs  
  let dims = 
       case args of
	  --[]      -> runMandel 1 1 3   -- Should output 57.
	  []      -> (4, 4, 3)   -- Should output 57.
	  [a,b,c] -> (read a, read b, read c)

  liftIO$ putStrLn$ "Running mandel with max row/col/depth: "++ show dims

  runMandel dims

  
-----------------------------------------------------------------------------

#if 0
type Pair = (Int16, Int16)

dynAPI = True -- TEMPTOGGLE

mandelProg :: Int -> Int -> Int -> GraphCode Int
mandelProg max_row max_col max_depth = 
    do --dat      :: ItemCol Pair (Complex Double) <- newItemCol
       pixel    :: ItemCol Pair Int              <- newItemCol
       
       let mandelStep tag@(i,j) = 
	    let z = (r_scale * (fromIntegral j) + r_origin) :+ 
  		    (c_scale * (fromIntegral i) + c_origin) in
	    do tid <- stepUnsafeIO myThreadId
	       --stepPutStr$ "["++ show tid ++"] Mandel Step executing: "++ show tag ++ "\n"
	       --cplx <- get dat tag
	       put pixel tag (mandel max_depth z)

       position :: TagCol  Pair <- prescribeNT [mandelStep] 

       initialize $ 
        forM_ [0..max_row-1] $ \i -> 
         forM_ [0..max_col-1] $ \j ->
          let (_i,_j) = (fromIntegral i, fromIntegral j)
	      z = (r_scale * (fromIntegral j) + r_origin) :+ 
  		  (c_scale * (fromIntegral i) + c_origin) in
	  do -- put dat (_i,_j) z
	     if dynAPI
	       then forkStep$ mandelStep (_i,_j)
	       else putt position (_i,_j)

       -- Final result, count coordinates of the  pixels with a certain value:
       finalize $ do 
        --stepPutStr$ "Finalize action begun...\n"
	foldM (\acc i -> 
          foldM (\acc j -> 
	           do --stepPutStr$ " ... try get pixel "++ show (i,j) ++"\n "
		      p <- get pixel (fromIntegral i, fromIntegral j)
		      --stepPutStr$ " GET PIXEL SUCCESSFUL "++ show (i,j) ++"\n "
		      if p == max_depth
   		       then return (acc + (i*max_col + j))
   		       else return acc)
	        acc [0..max_col-1]
              ) 0 [0..max_row-1] 
       
   where 
    r_origin = -2                            :: Double
    r_scale  = 4.0 / (fromIntegral max_row)  :: Double
    c_origin = -2.0                          :: Double
    c_scale = 4.0 / (fromIntegral max_col)   :: Double

#endif