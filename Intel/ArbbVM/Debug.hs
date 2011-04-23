{-# LANGUAGE ForeignFunctionInterface, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS  -XDeriveDataTypeable -fwarn-unused-imports #-}

module Intel.ArbbVM.Debug
  (
    DbgEvent(..),
    dbg, dbg0, dbgfile,
    runTrace, runReproducer,

    -- TEMP:
    newDBGFile, printInfo
  )
where 

import Debug.Trace
import Control.Concurrent
import Data.IORef
import Data.List
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
 | DbgCall { operator :: String,
	     operands :: [NamedValue],
	     result   :: NamedValue }

  -- TODO: Need to add actual array contents to the debug trace somehow.
   deriving Show

-- A printed value together with a descriptive name:
type NamedValue = (String,String)


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

dbg msg inputs (nom,accf) (ec, rv, ed) = 
  do     
     id     <- myThreadId
     new_tl <- newEmptyMVar
     let evt = DbgCall msg inputs (nom, show (accf rv))
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
     -- putStrLn$ "Wrote DBG event: "++ show evt

     -- TEMP: Keeping the old-style print messages for now as well:
     appendFile dbgfile $ 
        msg ++ concatMap printInfo inputs ++ 
        "-> {" ++ nom ++ " = " ++ show (accf rv) ++ " }" ++ "\n"   

     return (ec, rv, ed)

-- | Log a call to  a function without a return value.
dbg0 msg inputs (ec,ed) = 
 do
  (a,b,c) <- dbg msg inputs ("unit", id) (ec,(),ed) 
  return (a,c)


-- --------------------------------------------------------------------------------

-- | Generate a C file that reproduces a logged interaction with the
--   ArBB VM.  This function uses some dangerous heuristics and while
--   it is useful for debugging it should not be relied upon for
--   production purposes.

makeCReproducer :: [DbgEvent] -> String
makeCReproducer log = render doc
--  do let hndl = stdout 
--     hPutStrLn hndl "int main() {"
 where 
  doc = 
    text "#include <stdio.h>" $$ 
    text "int main() {" $$ 
    nest 4 (loop M.empty log) $$
    text "}\n" 

  loop mp [] = empty
  loop mp (DbgStart:tl) = loop mp tl
  loop mp (DbgCall oper rands result : tl) = 
    let rands' = map (text . show . snd) rands in 
    text oper <> 
    parens (hcat$ intersperse comma rands') <> 
    text ";" $$ 
    loop mp tl

newDBGFile x =
  do 
   b <- doesFileExist dbgfile 
   if b then removeFile dbgfile else return ()
   return x  
 

printInfo ::(String, String) -> String
printInfo (nom,val) = 
          "{" ++ nom ++ " = " ++ val ++ " }"

