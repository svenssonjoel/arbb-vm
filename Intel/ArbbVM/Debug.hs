{-# LANGUAGE ForeignFunctionInterface, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS  -XDeriveDataTypeable -fwarn-unused-imports #-}

{-

  TODO LIST:
============================================================

    * Arguments to "dbg" need to be tweaked/pruned so that they
      reflect ONLY the arguments to the actual C api calls. 

          o This refactoring is in-progress within ArBBVM.chs

          o The strings-only hack may not be sustainable.
          o Perhaps the next step would be to include more structure
            and metadata in the DbgEvent.  That's fine with me though --
            the main thing was that I wanted DbgEvent to have Show/Read
            invariance, not depend on in-memory haskell data
            structures...

    * The array capture mechanism needs to be implemented.  A
      DbgArraySnapshot needs to be created when an array is sent to
      ArBB (bind?), then makeCReproducer needs to generate a big
      constant C array.  OR another approach is to store each array in
      its binary representation in a file.

    * When the whole thing works I was going to factor out Joel's
      original debugging printed messages and implement a separate
      function that consumes DbgTraces and prints those same messages.

-}

module Intel.ArbbVM.Debug
  (
    DbgEvent(..), Param(..),
    outp, inp,
    dbg, dbg0, dbg2, dbgfile,
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

import System.IO
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
 | DbgCall { operator :: String
	   , operands :: [Param]
--	   ,  result   :: NamedValue 
	   }
 -- NOTE: This is an inefficent representation, we could at least use
 -- binary representations for common scalar types:
 -- Also we could use bytestrings here and Text.Show.ByteString:
 | DbgArraySnapshot [String]
   deriving (Show, Read)

-- A printed value together with a descriptive name:
type NamedValue = (String,String)


data Param = OutP String String
	   | InP  String String
  deriving (Show, Read)

-- Generic version:
outp s x = OutP s (show x)
inp  s x = InP  s (show x)

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
dbg2 msg params  = 
 do 
     id     <- myThreadId
     new_tl <- newEmptyMVar
     let evt = DbgCall msg params 
         loop = do
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


-- | Log a call to  a function without a return value.
dbg0 msg inputs (ec,ed) = 
 do
  (a,b,c) <- dbg msg inputs ("unit", id) (ec,(),ed) 
  return (a,c)


-- --------------------------------------------------------------------------------

type MyState a = State (M.Map String String, [String], Int) a

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
    text "#include <arbb_vmapi.h>" $$ 
    text "int main() {" $$ 
    nest 4 (loop 0 M.empty log) $$
    text "    printf(\"Reproducer finished.\\n\");" $$
    text "}\n" 

  deptr ty = case reverse ty of 
	      '*':tl -> reverse tl
	      _ -> error$ "expected pointer type to end in *: "++ ty

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

  -- Here we take a value represented as a STRING and put it in an acceptable C++ format.
  -- This is quite primitive, and totally a HACK.
  printValue ty str = 
   trace ("Printing C value from str: "++str)$ 
   case str of 
     -- Constants/ENUMs are replaced with their ArBB equivalent:
     'A':'r':'b':'b':_ -> return$ caseToUnderscore str

     -- Here's another hack for handling lists:
     -- TODO: this only works one level deep... would need better parsing to go further:
     -- TODO: One solution would simply be to move beyond using Strings for the value representation here!
     '[':tl -> 
       do cntr <- incr_cntr
          let chopped = take (length tl - 1) tl 
              elems   = splitOn "," chopped
              freshname = "arr" ++ show cntr
              elemty = deptr ty -- TOTAL HACK!
          chunks <- mapM (printValue elemty) elems 
          -- Here we use an array initializer:
          add_init $    
            elemty ++" "++ freshname ++
            "["++ show (length elems) ++ "] = {" ++ 
	      (concat $ intersperse ", " chunks)
	        ++ "};" 
          return freshname

     -- Numbers go right through:
     s | all isDigit s -> return s
     -- Pointers should have been mapped to a previous return value...
     s | isNullPtr s   -> return "NULL"

     s | isPtr s -> 
	-- error$ "makeCReproducer: unrecognized pointer value: "++s
        -- Here we check if we've already seen that ptr value before:
      do (mp,_,_) <- get
         case M.lookup s mp of
	  Nothing   -> return s
	  Just name -> return name

--     s           -> error$ "makeCReproducer: unrecognized printed value: "++s
     s           -> trace ("WARNING: makeCReproducer: unrecognized printed value: "++s) $ 
		    return s

  loop cntr mp [] = empty
  loop cntr mp (DbgStart:tl) = loop cntr mp tl
  loop cntr mp (DbgCall oper rands : tl) = 
    trace ("   Processing dbgcall, cntr "++ show cntr ++" "++ show mp) $ 
    let 
        stateM :: MyState [String] = 
	   sequence $ map dorand rands

        (rands', (mp2,inits,cntr2)) = runState stateM (mp,[],cntr) 

        -- dorand returns the arguments text.  It also modifies a map
	-- of seen values and a list of initialization statements that
	-- it passes as state.  
	dorand (InP ty r) = printValue ty r

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
    vcat (map text inits) $$ 
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
