


{- |  

   A module capturing common patterns in ArBB VM code emission and
   thereby making it easier to emit code.

 -}

module Intel.ArbbVM.Convenience 
 (
  ifThenElse,
  while,
  readScalarOfSize,
  newConstant
 )
where

import Intel.ArbbVM 
import Data.Serialize
import Data.ByteString.Internal
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Ptr 
import C2HS

import qualified  Control.Monad.State.Strict as S 

--------------------------------------------------------------------------------
-- The monad for emitting Arbb code.  
type EmitArbb = S.StateT ArbbEmissionState IO

-- We put the context and a stack of function types into the background.
type ArbbEmissionState = (Context, [Function])


--------------------------------------------------------------------------------
-- Convenience functions for common patterns:

-- ifThenElse  
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
