{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Data.Array.Accelerate.ArBB.Data where 

import Foreign.Ptr
import Data.Int
import Data.Word
--import Data.Maybe
--import Data.Word

import qualified Data.Array.Accelerate.Array.Data as AD

import Intel.ArbbVM
import Intel.ArbbVM.Convenience
------------------------------------------------------------------------------
-- Towards Binding Accelerate Arrays to ArBB Variables
class AD.ArrayElt e => ArrayElt e where 
   type APtr e
   bindArray :: AD.ArrayData e -> Int -> EmitArbb [Variable]

instance ArrayElt () where 
   type APtr () = Ptr ()      
   bindArray _ _ = return []  

#define primArrayElt_(ty,con)                                            \
instance ArrayElt ty where {                                        \
   type APtr ty = Ptr con                                                \
;  bindArray = bindArray' } 

#define primArrayElt(ty) primArrayElt_(ty,ty)
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



fst' :: AD.ArrayData (a,b) -> AD.ArrayData a
fst' = AD.fstArrayData

snd' :: AD.ArrayData (a,b) -> AD.ArrayData b
snd' = AD.sndArrayData



------------------------------------------------------------------------------ 
-- Print a message and then return a dummy (for now) 
bindArray' :: forall e a. (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e) => AD.ArrayData e -> Int -> EmitArbb [Variable]
bindArray' ad i = do
   liftIO$ putStrLn "hej" -- (show (getArray ad))
   res <- int32_ 42 -- Create a dummy variable 
   return [res]
 
------------------------------------------------------------------------------  
-- getArray turn a (Ptr a) to a wordPtr
getArray :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e) => AD.ArrayData e -> WordPtr
getArray = ptrToWordPtr . AD.ptrsOfArrayData 

