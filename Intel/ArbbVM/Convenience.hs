{-# LANGUAGE CPP #-}

{- |  

   A module capturing common patterns in ArBB VM code emission and
   thereby making it easier to emit code.

 -}

module Intel.ArbbVM.Convenience 
 -- (
 --  ifThenElse,
 --  while,
 --  readScalarOfSize,
 --  newConstant
 -- )
where

--import qualified Intel.ArbbVM as VM
import Intel.ArbbVM as VM

import Control.Monad
import Data.Serialize
import Data.ByteString.Internal
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Storable as Storable
import Foreign.Ptr 
import C2HS

import qualified  Control.Monad.State.Strict as S 

import Debug.Trace

--------------------------------------------------------------------------------
-- The monad for emitting Arbb code.  
type EmitArbb = S.StateT ArbbEmissionState IO

-- We put the context and a stack of function types into the
-- background.  Note, we need the stack of functions because we allow
-- nested function definitions at this convenience layer.  (They will
-- all have global scope to ArBB however.)
type ArbbEmissionState = (Context, [Function])
-- Note: if we also include a counter in here we can do gensyms...


#define L S.lift$

liftIO :: IO a -> EmitArbb a
liftIO = S.lift -- Allow the user to perform IO inside this monad.

arbbSession :: EmitArbb a -> IO a 
arbbSession m = 
  do ctx <- getDefaultContext
     (a,s) <- S.runStateT m (ctx,[])
     return a

getFun msg = 
 do (_,ls) <- S.get
    case ls of 
      [] -> error$ msg ++" when not inside a function"
      (h:t) -> return h


getCtx = 
 do (ctx,_) <- S.get
    return ctx

--------------------------------------------------------------------------------
-- Convenience functions for common patterns:

op_ :: Opcode -> [Variable] -> [Variable] -> EmitArbb ()
op_ code out inp = 
 do fun <- getFun "Convenience.op_ cannot execute an Opcode"
    L op fun code out inp

if_ :: Variable -> EmitArbb a -> EmitArbb a1 -> EmitArbb ()
if_ c t e =
  do fun <- getFun "Convenience.if_ cannot execute a conditional"
     L ifBranch fun c 
     t -- op myfun ArbbOpSub [c] [a,a]
     L elseBranch fun 
     e -- op myfun ArbbOpDiv [c] [a,a]
     L endIf fun

-- | An ArBB while loop.  Must be called inside a function definition.
while_ :: (EmitArbb Variable) -> EmitArbb a -> EmitArbb a
while_ cond body = 
   do fun <- getFun "Convenience.while_ cannot execute a while loop"
      L beginLoop fun ArbbLoopWhile
      L beginLoopBlock fun ArbbLoopBlockCond
      lc <- cond
      L loopCondition fun lc 
      L beginLoopBlock fun ArbbLoopBlockBody
      result <- body 
      L endLoop fun
      return result


const_ :: Storable a => ScalarType -> a -> EmitArbb Variable 
const_ st n = 
  do ctx <- getCtx
     L newConstantAlt ctx st n


readScalar_ :: (Num a, Storable a) =>  Variable -> EmitArbb a
readScalar_ v = 
  do ctx <- getCtx
     let z = 0
	 size = Storable.sizeOf z
     x <- L readScalarOfSize size ctx v 
     return (x+z)

type FunBody = [Variable] -> [Variable] -> EmitArbb ()

funDef_ :: String -> [Type] -> [Type] -> FunBody  -> EmitArbb Function
funDef_ name outty inty userbody = 
  do 
     ctx <- getCtx
     fnt     <- L getFunctionType ctx outty inty
     fun     <- L beginFunction ctx fnt name 0
     invars  <- L forM [0 .. length inty  - 1]   (getParameter fun 0)
     outvars <- L forM [0 .. length outty - 1]   (getParameter fun 1)

     -- Push on the stack:
     S.modify (\ (c,ls) -> (c, fun:ls))

     -- Now generate body:
     userbody outvars invars

     -- Pop off the stack:
     S.modify (\ (c, h:t) -> (c, t))
     L endFunction fun

     -- EXPERIMENTAL!  Compile immediately!!
     L compile fun

     return fun

call_ :: Function -> [Variable] -> [Variable] -> EmitArbb ()
call_ fun out inp = 
  do -- At the point of the call the *caller* is on the top of the stack:
     caller <- getFun "Convenience.call_ cannot call function"
     L callOp caller ArbbOpCall fun out inp


--------------------------------------------------------------------------------

-- These let us lift the slew of ArbbVM functions that expect a Context as a first argument.
lift1 :: (Context -> a -> IO b)           -> a           -> EmitArbb b
lift2 :: (Context -> a -> b -> IO c)      -> a -> b      -> EmitArbb c
lift3 :: (Context -> a -> b -> c -> IO d) -> a -> b -> c -> EmitArbb d

lift1 fn a     = do ctx <- getCtx; L fn ctx a 
lift2 fn a b   = do ctx <- getCtx; L fn ctx a b
lift3 fn a b c = do ctx <- getCtx; L fn ctx a b c

compile_ fn      = liftIO$ compile fn
execute_ a b c   = liftIO$ execute a b c

getBindingNull_  = liftIO getBindingNull

getScalarType_      = lift1 getScalarType
variableFromGlobal_ = lift1 variableFromGlobal
getFunctionType_    = lift2 getFunctionType
createGlobal_       = lift3 createGlobal
-- ... TODO ...  Keep going.


--------------------------------------------------------------------------------
-- OBSOLETE: These were some helpers that didn't use the EmitArbb monad.

ifThenElse :: Function -> Variable -> IO a -> IO a1 -> IO ()
ifThenElse f c t e =
  do
   ifBranch f c      
   t -- op myfun ArbbOpSub [c] [a,a]
   elseBranch f 
   e -- op myfun ArbbOpDiv [c] [a,a]
   endIf f


-- while loops
while :: Function -> (IO Variable) -> IO a1 -> IO ()
while f cond body = 
   do 
     beginLoop f ArbbLoopWhile
     beginLoopBlock f ArbbLoopBlockCond
     lc <- cond
     loopCondition f lc 

     beginLoopBlock f ArbbLoopBlockBody
     body 
     endLoop f     

-- Works not just for arrays but anything serializable:
withSerialized :: Serialize a => a -> (Ptr () -> IO b) -> IO b
withSerialized x fn =    
   withForeignPtr fptr (fn . castPtr)
 where 
   (fptr,_,_) = toForeignPtr (encode x)

newConstant :: Storable a => Context -> Type -> a -> IO Variable 
newConstant ctx t n = 
  do           
   -- Could use withSerialized possibly...
   tmp <- withArray [n] $ \x -> createConstant ctx t (castPtr x)
   variableFromGlobal ctx tmp

newConstantAlt :: Storable a => Context -> ScalarType -> a -> IO Variable 
newConstantAlt ctx st n = 
  do           
   t   <- getScalarType ctx st       
   tmp <- withArray [n] $ \x -> createConstant ctx t (castPtr x)
   variableFromGlobal ctx tmp

-- global/constant shortcuts

-- readScalarOfSize :: Storable b => Int -> Context -> Variable -> EmitArbb b
readScalarOfSize :: Storable b => Int -> Context -> Variable -> IO b
readScalarOfSize n ctx v = 
    allocaBytes n $ \ptr -> 
       do       
        readScalar ctx v ptr 
        peek (castPtr ptr)

-- TODO: readScalar of storable should be able to determine size.


--------------------------------------------------------------------------------
-- Complex numbers.