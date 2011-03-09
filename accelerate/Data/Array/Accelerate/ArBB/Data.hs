{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Data.Array.Accelerate.ArBB.Data where 

import Foreign.Ptr
import Data.Int
import Data.Word
--import Data.Maybe
--import Data.Word

import qualified Data.Array.Accelerate.Array.Data as AD
import           Data.Array.Accelerate.Array.Data (ArrayEltR(..)) 
import qualified Data.Array.Accelerate.Array.Sugar as Sugar

import Intel.ArbbVM
import Intel.ArbbVM.Convenience

import Data.Array.Accelerate.ArBB.Type


#define mkPrimDispatch(dispatcher,worker)                                   \
; dispatcher ArrayEltRint    = worker ArbbI32                                      \
; dispatcher ArrayEltRint8   = worker ArbbI8                                      \
; dispatcher ArrayEltRint16  = worker ArbbI16                                      \
; dispatcher ArrayEltRint32  = worker ArbbI32                                      \
; dispatcher ArrayEltRint64  = worker ArbbI64                                      \
; dispatcher ArrayEltRword   = worker ArbbU32                                      \
; dispatcher ArrayEltRword8  = worker ArbbU8                                      \
; dispatcher ArrayEltRword16 = worker ArbbU16                                      \
; dispatcher ArrayEltRword32 = worker ArbbU32                                      \
; dispatcher ArrayEltRword64 = worker ArbbU64                                      \
; dispatcher ArrayEltRfloat  = worker ArbbF32                                      \
; dispatcher ArrayEltRdouble = worker ArbbF64                                      \
; dispatcher ArrayEltRbool   = error "mkPrimDispatcher: ArrayEltRbool"      \
; dispatcher ArrayEltRchar   = error "mkPrimDispatcher: ArrayEltRchar"      \
; dispatcher _               = error "mkPrimDispatcher: not primitive"

bindArray :: AD.ArrayElt e => AD.ArrayData e -> Int -> EmitArbb [Variable]
bindArray ad n = doBind AD.arrayElt ad
  where 
    doBind :: ArrayEltR e -> AD.ArrayData e -> EmitArbb [Variable]
    doBind ArrayEltRunit             _  = return []
    doBind (ArrayEltRpair aeR1 aeR2) ad = do 
       v1 <- doBind aeR1 (fst' ad) 
       v2 <- doBind aeR2 (snd' ad) 
       return (v1 ++ v2)
    doBind aer                       ad = doBindPrim aer ad n 
     where 
      { doBindPrim :: ArrayEltR e -> AD.ArrayData e -> Int -> EmitArbb [Variable] 
      mkPrimDispatch(doBindPrim,bindArrayPrim)
      }

bindArrayPrim :: forall a e. (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a) => 
                 ScalarType -> 
                 AD.ArrayData e -> 
                 Int -> EmitArbb [Variable]
bindArrayPrim st ad n = do  
   liftIO$ putStrLn (show (getArray ad))        
   let wptr = getArray ad
   bin <- createDenseBinding_ (wordPtrToPtr wptr) 1 [fromIntegral n] [4 {- sizeof element -}]
   liftIO$ putStrLn (show st)
   sty <- getScalarType_ st   -- Not a Cheat anymore
   dty <- getDenseType_ sty 1  
   gin <- createGlobal_ dty "input" bin -- Cheat (why marked as cheat ?)
   v <- variableFromGlobal_ gin
   return [v]

fst' :: AD.ArrayData (a,b) -> AD.ArrayData a
fst' = AD.fstArrayData

snd' :: AD.ArrayData (a,b) -> AD.ArrayData b
snd' = AD.sndArrayData


getArray :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e) => AD.ArrayData e -> WordPtr
getArray = ptrToWordPtr . AD.ptrsOfArrayData 


 

------------------------------------------------------------------------------
-- Towards Binding Accelerate Arrays to ArBB Variables


{-
class AD.ArrayElt e => ArrayElt e where 
   type APtr e
   bindArray :: AD.ArrayData e -> Int -> EmitArbb [Variable]

instance ArrayElt () where 
   type APtr () = Ptr ()      
   bindArray _ _ = return []  

-- #define primArrayElt_(ty,con)                                            \
instance ArrayElt ty where {                                        \
   type APtr ty = Ptr con                                                \
;  bindArray = bindArray' } 

-- #define primArrayElt(ty) primArrayElt_(ty,ty)
primArrayElt(Int)
primArrayElt(Int8)
primArrayElt(Int16)
primArrayElt(Int32)
primArrayElt(Int64)

primArrayElt(Word)
primArrayElt(Word8)
primArrayElt(Word16)
primArrayElt(Word32)
primArrayElt(Word64)

-- FIXME:
-- CShort
-- CUShort
-- CInt
-- CUInt
-- CLong
-- CULong
-- CLLong
-- CULLong

primArrayElt(Float)
primArrayElt(Double)

-- FIXME:
-- CFloat
-- CDouble

-- FIXME:
-- No concrete implementation in Data.Array.Accelerate.Array.Data
--
instance ArrayElt Bool where
  type APtr   Bool = ()
  bindArray        = error "TODO: ArrayElt Bool"
 
instance ArrayElt Char where
  type APtr   Char = ()
  bindArray        = error "TODO: ArrayElt Char"
 
-- FIXME:
-- CChar
-- CSChar
-- CUChar


instance (ArrayElt a, ArrayElt b) => ArrayElt (a,b) where 
   type APtr (a,b) = (APtr a,APtr b)
   bindArray ad n = do 
                     let a = fst' ad
                         b = snd' ad
                     a' <- bindArray a n
                     b' <- bindArray b n
                     return (concat [a',b'])






------------------------------------------------------------------------------ 
-- Print a message and then return a dummy (for now) 
bindArray' :: forall e a. (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e) => AD.ArrayData e -> Int -> EmitArbb [Variable]
bindArray' ad i = do
   liftIO$ putStrLn (show (getArray ad))        
   let wptr = getArray ad
   bin <- createDenseBinding_ (wordPtrToPtr wptr) 1 [fromIntegral i] [4 {- sizeof element -}]
   sty <- getScalarType_ ArbbI32   -- Cheat
   dty <- getDenseType_ sty 1      -- Cheat
   gin <- createGlobal_ dty "input" bin -- Cheat
   v <- variableFromGlobal_ gin
   -- res <- int32_ 42 -- Create a dummy variable 
   return [v]
 
------------------------------------------------------------------------------  
-- getArray turn a (Ptr a) to a wordPtr

-}




{-
bindArray' :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e) => AD.ArrayData e -> Int -> EmitArbb [Variable]
bindArray' ad i = do
   liftIO$ putStrLn (show (getArray ad))        
   let wptr = getArray ad
   bin <- createDenseBinding_ (wordPtrToPtr wptr) 1 [fromIntegral i] [4 {- sizeof element -}]
   sty <- getScalarType_ ArbbI32   -- Cheat
   dty <- getDenseType_ sty 1      -- Cheat
   gin <- createGlobal_ dty "input" bin -- Cheat
   v <- variableFromGlobal_ gin
   -- res <- int32_ 42 -- Create a dummy variable 
   return [v]
 -}