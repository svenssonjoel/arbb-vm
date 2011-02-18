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
import Data.Serialize
import Data.ByteString.Internal
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Storable as Storable
import Foreign.Ptr 
import C2HS

import qualified  Control.Monad.State.Strict as S 

--------------------------------------------------------------------------------
-- The monad for emitting Arbb code.  
type EmitArbb = S.StateT ArbbEmissionState IO

-- We put the context and a stack of function types into the
-- background.  Note, we need the stack of functions because we allow
-- nested function definitions at this convenience layer.  (They will
-- all have global scope to ArBB however.)
type ArbbEmissionState = (Context, [Function])

--------------------------------------------------------------------------------
-- Convenience functions for common patterns:

#define L S.lift$

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

readScalar_ :: (Num a, Storable a) =>  Variable -> EmitArbb a
readScalar_ v = 
  do (ctx,_) <- S.get
     let z = 0
	 size = Storable.sizeOf z
     x <- L readScalarOfSize size ctx v 
     return (x+z)



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

-- fun Defs
-- funDef :: Type -> Syntax -> [Type] -> ([Syntax] -> EasyEmit ()) -> EasyEmit ObjFun

--funDef :: Type -> Syntax -> [Type] -> ([Variable] -> EmitArbb ()) -> EmitArbb ObjFun
--funDef = undefined


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
