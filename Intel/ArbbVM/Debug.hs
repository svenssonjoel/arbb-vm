{-# LANGUAGE ForeignFunctionInterface, BangPatterns, ScopedTypeVariables, CPP #-}
{-# OPTIONS  -XDeriveDataTypeable -fwarn-unused-imports #-}

{-

  TODO LIST:
============================================================

    * Arguments to "dbg" need to be tweaked/pruned so that they
      reflect ONLY the arguments to the actual C api calls. 

          o This refactoring is in-progress within ArBBVM.chs
 
    * When the whole thing works I was going to factor out Joel's
      original debugging printed messages and implement a separate
      function that consumes DbgTraces and prints those same messages.

-}

module Intel.ArbbVM.Debug
  (
    DbgEvent(..), Param(..), Val(..),
    -- OLD:
    dbg, dbg0, dbgfile,
    -- NEW:
    dbg2, dbg_log_evt, dbg_snapshot, mk_snapshot,
    runTrace, runReproducer, makeCReproducer,

    -- TEMP:
    printInfo
  )
where 

import Control.Monad.State
import Control.Concurrent
import Debug.Trace
import Data.IORef
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Map as M

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI

import Foreign.Ptr

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Directory
import Text.PrettyPrint.HughesPJ

import C2HS hiding (sizeOf) 

-- --------------------------------------------------------------------------------
-- Globals and Datatype Definitions
-- --------------------------------------------------------------------------------

-- | Flag to disable debugging.  For now this is set statically in the
-- code.  It should be dynamically configurable.
debug_arbbvm = True

-- | Name of the debug output file, to be placed in the current directory.
dbgfile = "debugtrace_HaskellArBB.txt"

-- | A running ArBBVM session creates a trace of debug messages that
--   can be consumed immediately or accumulated in memory.
data DbgTrace = DbgCons Int TaggedDbgEvent (MVar DbgTrace)

-- Tagged with additional message and ThreadID:
type TaggedDbgEvent = (ThreadId,DbgEvent)

data DbgEvent = 
   DbgStart -- A dummy event.

 --  Log a call through the VMAPI:
 | DbgCall { operator :: String
	   , operands :: [Param]
--	   ,  result   :: NamedValue 
	   }

 -- Log a value returned FROM ArBB and store a copy of the value.
 | DbgReadAndCompare { origptr :: WordPtr
		     , snapshot :: B.ByteString
		     }
 deriving (Show, Read)

-- A printed value together with a descriptive name:
type NamedValue = (String,String)


data Param = OutP String Val
	   | InP  String Val
  deriving (Show, Read)

-- A simple value representation that retains some structure (before
-- this it was just strings).
data Val = VNum Int
	 | VStr String
	 | VPtr WordPtr

	 -- This represents a snapshot of a binary object in memory.
	 -- It's location (pointer) is also recorded for faster
	 -- equivalence checking.  WARNING -- long running
	 -- computations that free and malloc memory repeatedly could
	 -- pose a problem!
	 | VCapture WordPtr B.ByteString
         -- A placeholder for a fresh empty buffer of given size:
	 -- Also stores the corresponding pointer in the original execution:
         | VEmpty Int WordPtr 

	 | VEnum String
	 | VArr [Val]
  deriving (Show, Read, Ord, Eq)

-- Generic version:
-- outp s x = OutP s (show x)
-- inp  s x = InP  s (show x)

fromParam (OutP p v) = (p,v)
fromParam (InP  p v) = (p,v)


-- --------------------------------------------------------------------------------
-- Misc helper functions.

caseToUnderscore :: String -> String
caseToUnderscore lexeme =
  result
 where
    result = map toLower $ 
	     concat $
	     intersperse "_" words
    words = groupBy (\ _ b -> isLower b || isDigit b) lexeme

-- --------------------------------------------------------------------------------

-- Run an computation that interacts with the ArBB-VM and capture its
-- trace. 
runTrace :: IO a ->IO (a,[DbgEvent])
runTrace m = 
  do
     -- ASSUMPTION: m is a single threaded computation!
     tid <- myThreadId
     -- Capture the starting point for this segment of trace:
     head <- readIORef global_dbg_trace_tail
     -- Run the computation:
     x <- m 
     -- Capture the ending point:
     DbgCons cntr2 _ _ <- readIORef global_dbg_trace_tail
     -- Read everything inbetween:
     ls <- extractTrace tid cntr2 head
     return (x,ls)

reproducer_file = "reproducer.c"

-- Run an ArBBVM computation, log interactions, and generate a reproducer C program.
runReproducer :: IO a ->IO a
runReproducer m = do
  putStrLn$ "Begin logging ArBB-VM interactions and generating reproducer: " ++ reproducer_file
  (x,ls) <- runTrace m
  writeFile reproducer_file (makeCReproducer ls)
  putStrLn$ "Done writing reproducer to: "++reproducer_file
  return x

extractTrace :: ThreadId -> Int -> DbgTrace -> IO [DbgEvent]
extractTrace tid end trace = loop trace [] 
 where 
  loop (DbgCons ctr (id,evt) tl) !acc =
    if ctr == end then 
      return (reverse (evt:acc))
    else do nxt <- readMVar tl
	    loop nxt (if id==id then evt:acc else acc)


-- TODO: ASYNC VERSION:
-- Here the trace is returned as a lazy list so that it may be
-- consumed concurrently with the IO computation.
-- runWithTraceAsync :: IO a -> ([DbgEvent], IO a)

-- TOFIX: Presently debug traces are accumulated via a global
-- variable.  In the future it's probably better that this go in a
-- state monad!
global_dbg_trace_tail :: IORef DbgTrace
global_dbg_trace_tail = unsafePerformIO initial
  where initial = do tl <- newEmptyMVar
		     id <- myThreadId
	             newIORef (DbgCons 0 (id,DbgStart) tl)

-- | IO function to log an event in the debug trace.
dbg :: (Show c) => 
       String -> 
       [(String,String)] -> 
       (String, b -> c) -> 
--       (Error, b, ErrorDetails) -> IO (Error, b, ErrorDetails)
       (d, b, e) -> IO (d, b, e)

dbg msg inputs (nom,accf) orig@(ec, rv, ed) = 
  do     
-- TODO: DELETEME
     return orig


-- New version of logging procedure... refactoring.
dbg2 :: String -> [Param] -> IO ()
dbg2 msg params = dbg_log_evt (DbgCall msg params)

dbg_log_evt evt = 
 do id     <- myThreadId
    new_tl <- newEmptyMVar
    let loop = do
	    -- Now to add a new entry to the debug trace.  Things get tricky
	    -- because we want to do TWO things, extend the linked list and
	    -- modify the global variable to point to the new tail.  We could
	    -- use TVars to do that atomically but the following protocol
	    -- works as well.  Which runs better under contention?
	    DbgCons cntr hd tl <- readIORef global_dbg_trace_tail
	    let newcell = DbgCons (cntr+1) (id,evt) new_tl
	    success <- tryPutMVar tl newcell
	    if success then 
	     -- If we succeed then we have the right to repoint the global:
	     writeIORef global_dbg_trace_tail newcell
	     -- If we fail to fill the tail then someone else beat us to it and we retry:
	     else loop
    loop 


mk_snapshot  :: (Ptr a,Int) -> IO B.ByteString
mk_snapshot (ptr,sz) = do
   -- Allocate space for a snapshot:
   fptr <- BI.mallocByteString sz
   withForeignPtr fptr$ \dest -> do 
      BI.memcpy dest (castPtr$ ptr) (fromIntegral sz)
   return (BI.fromForeignPtr fptr 0 4)

-- Here's a wrapper which also snapshots a blob of data:
-- It should be disabled when debugging is off.
dbg_snapshot :: (Ptr a,Int) -> String -> (B.ByteString -> [Param]) -> IO ()
dbg_snapshot pr str fn = do
   -- TODO FIXME: Make debugging CONDITIONAL!!  Otherwise this is needlessly inefficient.
   snap <- mk_snapshot pr
   dbg2 str (fn snap)

-- | Log a call to  a function without a return value.
dbg0 msg inputs (ec,ed) = 
 do
  (a,b,c) <- dbg msg inputs ("unit", id) (ec,(),ed) 
  return (a,c)


-- --------------------------------------------------------------------------------

type MyState a = State (M.Map Val String, [String], Int) a

-- | Generate a C file that reproduces a logged interaction between
--   Haskell and the ArBB VM.  This function uses some dangerous
--   heuristics and while it is useful for debugging it should not be
--   relied upon for production purposes.

makeCReproducer :: [DbgEvent] -> String
makeCReproducer log = render doc
--  do let hndl = stdout 
--     hPutStrLn hndl "int main() {"
 where 
  doc = 
    text "#include <stdio.h>" $$ 
    text "#include <stdlib.h>" $$ 
    text "#include <arbb_vmapi.h>" $$ 

    text "char* read_file(const char* filename, int n) { \n\
            char* buffer = (char*)malloc(n);   \n\
            FILE* f = fopen(filename, \"rb\"); \n\
            if (f) {                           \n\
               fread(buffer, n, 1, f);         \n\
               return buffer;                  \n\
            }                                  \n\
            printf(\"Could not open file: %s\\n\",filename); abort(); }"   $$
    text ""$$
    text "int main() {" $$ 
    text "   arbb_binding_t null_binding; arbb_set_binding_null(&null_binding);" $$
    nest 4 (loop 0 M.empty log) $$
    text "    printf(\"Reproducer finished.\\n\");" $$
    text "}\n" 

  deptr ty = case reverse ty of 
	      '*':tl -> reverse tl
	      _ -> error$ "expected pointer type to end in *: "++ ty
  isPtrTy ty = case reverse ty of 
	        '*':tl -> True
		_      -> False

  -- Modify one field of the state stored in the state monad:
  add_init :: String -> MyState ()
  add_init new = 
    do (mp,inits,cntr) <- get
       put (mp, new:inits, cntr)

  incr_cntr :: MyState Int
  incr_cntr = 
    do (mp,inits,cntr) <- get
       put (mp, inits, cntr+1)
       return cntr

  printVal ty val = 
   case val of 
     -- Constants/ENUMs are replaced with their ArBB equivalent:
     VEnum str -> return$ caseToUnderscore str

     -- Here's another hack for handling lists:
     -- TODO: this only works one level deep... would need better parsing to go further:
     VArr ls -> 
       do cntr <- incr_cntr
          let 
              freshname = "arr" ++ show cntr
              elemty = deptr ty -- TOTAL HACK!
          chunks <- mapM (printVal elemty) ls
          -- Here we use an array initializer:
          add_init $    
            elemty ++" "++ freshname ++
            "["++ show (length ls) ++ "] = {" ++ 
	      (concat $ intersperse ", " chunks)
	        ++ "};" 
          return freshname

     -- -- Numbers go right through:
     VNum n -> return (show n)
     VStr s -> return (show s)

     -- Pointers should have been mapped to a previous return value...
     VPtr p | p == 0 && isPtrTy ty -> return "NULL"

     -- HACK: TODO: GENERALIZE
     VPtr p | p == 0 && ty == "arbb_binding_t" -> return "null_binding"

     VPtr p | p == 0 -> 
       error$ "UNIMPLEMENTED: Not sure what to do... null valued object passed by value with type: "++ty

     val@(VPtr ptr) -> 
     	-- error$ "makeCReproducer: unrecognized pointer value: "++s
        -- Here we check if we've already seen that ptr value before:
      do (mp,_,_) <- get
         case M.lookup val mp of
     	  Nothing   -> trace ("WARNING: unknown pointer: "++ show val) $
		       return (show$ wordPtrToPtr ptr)
     	  Just name -> return name

     VEmpty size origptr ->
       do cntr <- incr_cntr
	  let name = "buf"++show cntr
	  add_init$ "char* "++name++" = (char*)malloc("++show size++");"

-- TODO: Extend map connecting the name to the value of the origptr

	  return name

     VCapture p dat -> 
       do 
          cntr <- incr_cntr
	  let name = "snapshot"++show cntr
	      size = B.length dat

          -- HEURISTIC!  Larger files get dumped to disk, smaller ones encoded in text:
          if size <= 1000
	   then add_init$ "char "++ name ++ "["++ show size ++"] = {"++ 
		          concat (intersperse ", " (map show$ B.unpack dat)) ++"};"
           else do
	     let filename = "snapshot"++show cntr++".bin"
	     -- TEMP FIXME: DOING THIS UNSAFELY FOR NOW:
	     -- TODO: NEED TO ADD AN EXTRA ACCUMULATOR TO THE STATE
	     seq (unsafePerformIO (B.writeFile filename dat)) $ 
	       add_init$ "char* "++ name ++ " = read_file("++ show filename ++", "++
			 show size ++ ");"

          return name

     _ -> error $ "makeCReproducer: unhandled Val: "++ show val



  loop cntr mp [] = empty
  loop cntr mp (DbgStart:tl) = loop cntr mp tl

  loop cntr mp (DbgReadAndCompare ptr snapshot : tl) = 
   text ("// READ AND COMPARE RESULT "++ show (ptr,snapshot) ++"\n") $$
   loop cntr mp tl

  loop cntr mp (DbgCall oper rands : tl) = 
    let 
        stateM :: MyState [String] = 
	   sequence $ map dorand rands

        (rands', (mp2,inits,cntr2)) = runState stateM (mp,[],cntr) 

        -- dorand returns the arguments text.  It also modifies a map
	-- of seen values and a list of initialization statements that
	-- it passes as state.  
	dorand (InP ty r) = printVal ty r

        -- Output parameters need to be handled differently, we must
        -- allocate space for them.  We assume they are of pointer type.
	dorand (OutP ty r) = 
          do (mp0,inits,cntr) <- get
	     let uid = -- trace ("outP "++ show (r) ++" Size of map "++ show (M.size mp0) ++" "++ show mp0) $ 
		       -- M.size mp0 
		       cntr
		 freshname = "p" ++ show uid
		 -- Add new map entry and initialization expression:
		 mp'    = M.insert r freshname mp0
		 inits' = (deptr ty ++ " "++ freshname ++";") : inits

	     put (mp', -- if isNullPtr r then mp' else mp0, 
		  inits', cntr+1)
	     return ("&" ++ freshname)

    in 
    vcat (map text$ reverse inits) $$ 
    text oper <> 
      parens (hcat$ intersperse (comma<>space) (map text rands')) <> 
      text ";" $$ 
    loop cntr2 mp2 tl

-- Hackish:
isPtr = isPrefixOf "0x"
isNullPtr ('0':'x': tl) = all (=='0') tl
isNullPtr _             = False
 
printInfo ::(String, String) -> String
printInfo (nom,val) = 
          "{" ++ nom ++ " = " ++ val ++ " }"