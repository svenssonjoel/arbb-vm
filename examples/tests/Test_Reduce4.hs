{-# LANGUAGE CPP, ScopedTypeVariables, CPP #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import Data.Word

import Data.Time


{- 
   Experiment on more general reductions. 
    - not limited to pow2 size 
    - should work for 1,2,3D arrays (in the way expected by Accelerate) 
    - In future for for N-Dimensional Arrays.  

   
   TODO: 
    - execute_ is asynchronous may enable me to do what I want 
      (but spread out over many calls into ArBB) 

  
-}

newGlobalVar t nom = do 
  bin <- getBindingNull_ 
  g <- createGlobal_ t nom bin
  variableFromGlobal_ g     


genRed reducer out inp n = do         
   sty <- getScalarType_ ArbbI32
   size_t  <- getScalarType_ ArbbUsize
   dsize_t <- getDenseType_ size_t 1
   dty <- getDenseType_ sty 1
   nty <- getNestedType_ sty  
   

   indices <- newGlobalVar dsize_t "indices" 
   chunk   <- newGlobalVar size_t "chunk_size" 
   nested  <- newGlobalVar nty "chunk"         
   
   one_chunk <- newGlobalVar dty "achunk" 
   
   chunks <- mapM (newGlobalVar dty) ["c" ++ show n | n <- [0..9]]  
   tmp    <- mapM (newGlobalVar sty) ["t" ++ show n | n <- [0..9]]  

   liftIO$ putStrLn "A"
   ten <- usize_ 10
   zero <- usize_ 0
   one <- usize_ 1
   two <- usize_ 2
   three <- usize_ 3
   four <- usize_ 4
   five <- usize_ 5
   six <- usize_ 6
   seven <- usize_ 7
   eight <- usize_ 8 
   nine  <- usize_ 9
   
   vs <- usize_ 2  -- This is a Enum VS_OFFSET
        
   liftIO$ putStrLn "B"
   opImm_ ArbbOpDiv [chunk] [n,ten] 
               
   liftIO$ putStrLn "C"
   opDynamicImm_ ArbbOpIndex [indices] [zero,ten,chunk] -- ten chunks! 

   liftIO$ putStrLn "D"
   -- When it is Dynamic or not, makes no sense to me !!! 
   opImm_ ArbbOpApplyNesting [nested] [inp,indices,vs]
              
   liftIO$ putStrLn "E"      
   
   -- extracts a segment (a dense from a nested)    
   opImm_ ArbbOpSegment [chunks !! 0] [nested,zero] 

   --liftIO$ putStrLn "F"
   execute_ reducer [tmp !! 0] [chunks !! 0,chunk]          
          
   opImm_ ArbbOpSegment [chunks !! 1] [nested,one] 

   --liftIO$ putStrLn "F1"
   execute_ reducer [tmp !! 1] [chunks !! 1,chunk]          
   
   opImm_ ArbbOpSegment [chunks !! 2] [nested,two] 

   --liftIO$ putStrLn "F2"
   execute_ reducer [tmp !! 2] [chunks !! 2,chunk]          

   opImm_ ArbbOpSegment [chunks !! 3] [nested,three] 

   --liftIO$ putStrLn "F3"
   execute_ reducer [tmp !! 3] [chunks !! 3,chunk]          

   opImm_ ArbbOpSegment [chunks !! 4] [nested,four] 
   --liftIO$ putStrLn "F4"
   execute_ reducer [tmp !! 4] [chunks !! 4,chunk]          

   opImm_ ArbbOpSegment [chunks !! 5] [nested,five] 
   --liftIO$ putStrLn "F5"
   execute_ reducer [tmp !! 5] [chunks !! 5,chunk]          

   opImm_ ArbbOpSegment [chunks !! 6] [nested,six] 
   --liftIO$ putStrLn "F6"
   execute_ reducer [tmp !! 6] [chunks !! 6,chunk]          
   
   opImm_ ArbbOpSegment [chunks !! 7] [nested,seven] 
   --liftIO$ putStrLn "F7"
   execute_ reducer [tmp !! 7] [chunks !! 7,chunk]          
   
   opImm_ ArbbOpSegment [chunks !! 8] [nested,eight] 
   --liftIO$ putStrLn "F8"
   execute_ reducer [tmp !! 8] [chunks !! 8,chunk]          
  
   opImm_ ArbbOpSegment [chunks !! 9] [nested,nine] 
   opImm_ ArbbOpLength  [chunk] [chunks !! 9]
   --liftIO$ putStrLn "F9"
   execute_ reducer [tmp !! 9] [chunks !! 9,chunk]          


   opImm_ ArbbOpAdd [tmp !! 0] [tmp !! 0, tmp !! 1]
   opImm_ ArbbOpAdd [tmp !! 0] [tmp !! 0, tmp !! 2] 
   opImm_ ArbbOpAdd [tmp !! 0] [tmp !! 0, tmp !! 3]
   opImm_ ArbbOpAdd [tmp !! 0] [tmp !! 0, tmp !! 4]
   opImm_ ArbbOpAdd [tmp !! 0] [tmp !! 0, tmp !! 5] 
   opImm_ ArbbOpAdd [tmp !! 0] [tmp !! 0, tmp !! 6]
   opImm_ ArbbOpAdd [tmp !! 0] [tmp !! 0, tmp !! 7]
   opImm_ ArbbOpAdd [tmp !! 0] [tmp !! 0, tmp !! 8] 
   opImm_ ArbbOpAdd [tmp !! 0] [tmp !! 0, tmp !! 9]
   
   opImm_ ArbbOpCopy [out] [tmp !! 0]

main = arbbSession$ do 
     sty <- getScalarType_ ArbbI32
     dty <- getDenseType_ sty 1
     dty2 <- getDenseType_ sty 2 
     bt <- getScalarType_ ArbbBoolean
     
     nty <- getNestedType_ sty 
     
     size_t <- getScalarType_ ArbbUsize
     dsize_t <- getDenseType_ size_t 1

     add <- funDef_ "add" [sty] [sty,sty] $ \ [out] [i1,i2] -> do 
        op_ ArbbOpAdd [out] [i1,i2]
        
     -- First 1D to 0D case.. then more involved ones! 
     seqRed <- funDef_ "seqRed" [sty] [dty,size_t] $ \ [out] [inp,n] -> do 
        
     --   n <- createLocal_ size_t "length" 
        currs <- createLocal_ size_t "currs" 
        res <- createLocal_ sty "result"   
        elm <- createLocal_ sty "element"      
        c   <- createLocal_ bt  "condition"
        
        zero <- usize_ 0 
        one <- usize_ 1
        
        
      --  op_ ArbbOpLength  [n] [inp] 
        op_ ArbbOpCopy    [currs] [one] -- initiate loop counter to zer0
        opDynamic_ ArbbOpExtract [res] [inp,zero]
        
        while_  -- loop from 0 to n 
         (do
           op_ ArbbOpLess [c] [currs,n]   
           return c
         ) 
         (do    
           opDynamic_ ArbbOpExtract [elm] [inp,currs]
           call_ add [res] [res,elm] 
           op_ ArbbOpAdd [currs] [currs,one] 
         ) 
        op_ ArbbOpCopy [out] [res]
        
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
     -- withArray_ (replicate 8 0 :: [Word32]) $ \ out -> 
       do

        inb <- createDenseBinding_ (castPtr inp) 1 [2^24] [4]
       -- outb <- createDenseBinding_ (castPtr out) 1 [8] [4]
       
        gin <- createGlobal_ dty "input" inb
       -- gout <- createGlobal_ dty "output" outb
       
        vin <- variableFromGlobal_ gin
       -- vout <- variableFromGlobal_ gout
       
        n <- usize_ (2^24)
        binding <- getBindingNull_
        g       <- createGlobal_ sty "res" binding
        y       <- variableFromGlobal_ g
        --execute_ reduceStep [vout] [vin,n]     
    
        t1 <- liftIO getCurrentTime                          
        -- execute_ reduce [y] [vin,n]
        -- execute_ seqRed [y] [vin,n]
        --execute_ genRed [y] [vin,n]
        genRed seqRed y vin n
        finish_
        t2 <- liftIO getCurrentTime 

----------
        
        --str <- serializeFunction_ reduceStep
        --liftIO$ putStrLn (getCString str)
        --str <- serializeFunction_ reduce
        --liftIO$ putStrLn (getCString str)
        -- str <- serializeFunction_ seqRed
        --liftIO$ putStrLn (getCString str)

---------  


        result :: Word32 <- readScalar_ y      

        liftIO$ putStrLn $ "time: " ++ ( show (diffUTCTime t2 t1) )  
        --result <- liftIO $ peekArray 8 (castPtr out :: Ptr Word32) 
        liftIO$ putStrLn $ show result
         
        
     