{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Data.Array.Accelerate.ArBB.Data where 

import Foreign.Ptr
import Data.Int
import Data.Word
import Data.Typeable

import qualified Data.Array.Accelerate.Array.Data as AD
import           Data.Array.Accelerate.Array.Data (ArrayEltR(..)) 
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import           Data.Array.Accelerate.Array.Sugar (Array(..))
import           Data.Array.Accelerate.AST
import Data.Array.Accelerate.ArBB.Type
import Data.Array.Accelerate.Array.Representation

import           Intel.ArbbVM
import           Intel.ArbbVM.Convenience
import qualified Intel.ArbbVM.Type as ArBB 


import Foreign.Marshal.Array
import Foreign.Marshal.Utils

import Control.Monad.ST

import qualified Data.Map as M 
------------------------------------------------------------------------------
-- 

-- Array to name bindigs
type GlobalBindings a = M.Map (Ptr ()) a 

data ArrayDesc = ArrayDesc { arrayDescLength :: Word64, 
                             arrayDescType   :: ScalarType } 


------------------------------------------------------------------------------
-- The accelerate guys seem to love these CPP hacks (im trying to keep up)
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

------------------------------------------------------------------------------
-- small helpers. 
fst' :: AD.ArrayData (a,b) -> AD.ArrayData a
fst' = AD.fstArrayData

snd' :: AD.ArrayData (a,b) -> AD.ArrayData b
snd' = AD.sndArrayData


getArray :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e) => AD.ArrayData e -> Ptr ()
getArray = castPtr . AD.ptrsOfArrayData 


 
------------------------------------------------------------------------------
-- Collect global bindings

collectGlobals :: (Typeable aenv, Typeable a) =>        
                  OpenAcc aenv a -> 
                  GlobalBindings ArrayDesc ->  
                  GlobalBindings ArrayDesc
collectGlobals acc@(OpenAcc pacc) gb =
     case pacc of 
       (Use (Array sh ad)) -> 
         let n = size sh 
         in insertArray ad (fromIntegral n) gb  
       (Map f acc) -> collectGlobals acc gb 

------------------------------------------------------------------------------
-- insertArray

insertArray :: AD.ArrayElt e => 
               AD.ArrayData e -> Word64 -> 
               GlobalBindings ArrayDesc -> 
               GlobalBindings ArrayDesc 
insertArray ad n gb = doInsert AD.arrayElt ad gb
  where 
    doInsert :: ArrayEltR e -> 
                AD.ArrayData e ->      
                GlobalBindings ArrayDesc -> 
                GlobalBindings ArrayDesc 
    doInsert ArrayEltRunit             _  gb = gb
    doInsert (ArrayEltRpair aeR1 aeR2) ad gb =  
       let gb'  = doInsert aeR1 (fst' ad) gb
           gb'' = doInsert aeR2 (snd' ad) gb'
       in gb''
    doInsert aer                       ad gb = doInsertPrim aer ad n gb
     where 
      { doInsertPrim :: ArrayEltR e -> AD.ArrayData e -> Word64 -> GlobalBindings ArrayDesc -> GlobalBindings ArrayDesc
      mkPrimDispatch(doInsertPrim,insertArrayPrim)
      }

insertArrayPrim :: forall a e. (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a) => 
                 ScalarType -> 
                 AD.ArrayData e -> 
                 Word64 -> GlobalBindings ArrayDesc ->
                 GlobalBindings ArrayDesc 
insertArrayPrim st ad n gb = 
   -- let ptr = wordPtrToPtr (getArray ad)
   let ptr = getArray ad
   in case M.lookup  ptr gb  of 
        (Just _) -> gb
        Nothing ->  
          let arrd = ArrayDesc n st 
          in M.insert ptr arrd gb
 
------------------------------------------------------------------------------
-- lookupArray
lookupArray :: AD.ArrayElt e => 
               AD.ArrayData e ->  
               GlobalBindings Variable -> 
               [Variable] 
lookupArray ad gv = doLookup AD.arrayElt ad gv
  where 
    doLookup :: ArrayEltR e -> 
                AD.ArrayData e ->      
                GlobalBindings Variable -> 
                [Variable] 
    doLookup ArrayEltRunit             _  gv = []
    doLookup (ArrayEltRpair aeR1 aeR2) ad gv =  
       let v1 = doLookup aeR1 (fst' ad) gv
           v2 = doLookup aeR2 (snd' ad) gv
       in v1 ++ v2
    doLookup aer                       ad gv = doLookupPrim aer ad gv
     where 
      { doLookupPrim :: ArrayEltR e -> AD.ArrayData e -> GlobalBindings Variable -> [Variable]
      mkPrimDispatch(doLookupPrim,lookupArrayPrim)
      }

lookupArrayPrim :: forall a e. (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a) => 
                 ScalarType -> 
                 AD.ArrayData e -> 
                 GlobalBindings Variable ->
                 [Variable] 
lookupArrayPrim st ad  gv = 
   --let ptr = wordPtrToPtr (getArray ad)
   let ptr = getArray ad
   in case M.lookup  ptr gv  of 
        (Just v) -> [v]
        Nothing -> error "LookupArrayPrim: Implementation of ArBB backend is faulty!" 
          

------------------------------------------------------------------------------
-- bindGlobals

bindGlobals :: GlobalBindings ArrayDesc -> EmitArbb (GlobalBindings Variable)
bindGlobals gb = doBindGlobals (M.toList gb) M.empty 
  where 
    doBindGlobals [] gv = return gv
    doBindGlobals ((ptr,arrd):xs) gv = do 
       let n = arrayDescLength arrd
           st = arrayDescType arrd
--- DEBUG   
       liftIO$ putStrLn (show n)
       liftIO$ putStrLn (show st)
       array <- liftIO$ peekArray 10 (castPtr ptr)  
       liftIO$ putStrLn (show (array :: [Int32]))    
       
-- ArBB CALLS 

       let name = "input" ++ show ptr
       liftIO$ putStrLn ("   To variable: "  ++ name)
      
       bin <- getBindingNull_
       sty <- getScalarType_ st   
       dty <- getDenseType_ sty 1  
       gin <- createGlobal_ dty name bin 
       -- g_apa <- createGlobal_ dty "in" bin
       v <- variableFromGlobal_ gin
       
       num_elems <- usize_ n 
        
       opDynamicImm_ ArbbOpAlloc [v] [num_elems]
     
       m_ptr <- mapToHost_ v [1] ArbbReadWriteRange
       liftIO$ copyBytes m_ptr ptr ((fromIntegral n) * (ArBB.size st)) 
             
   
-------------
       
       gv' <- doBindGlobals xs gv
       let gvout = M.insert ptr v gv' 
       return gvout




  
------------------------------------------------------------------------------
-- 
data InternalArray sh where 
     InternalArray :: (Sugar.Shape sh) 
                   => Sugar.EltRepr sh 
                   -> Vars             -- bound to ArBB Variables 
                   -> InternalArray sh

data Vars  
   = VarsUnit  
   | VarsPrim Variable
   | VarsPair Vars Vars 
   deriving Show 

-- depth first order (If I am not mistaken) 
varsToList VarsUnit =[] 
varsToList (VarsPrim v)  = [v] 
varsToList (VarsPair v1 v2) = varsToList v1 ++ varsToList v2

data Result arrs where 
  ResultUnit :: Result () 
  ResultArray :: (Sugar.Shape sh, Sugar.Elt e) => InternalArray sh -> Result (Array sh e) 
  ResultPair :: Result a1 -> Result a2 -> Result (a1,a2)

-- again depth first order !
resultToVList ::Result  a -> [Vars] 
resultToVList ResultUnit = [] 
resultToVList (ResultArray (InternalArray _ vars)) = [vars] 
resultToVList (ResultPair r1 r2) = resultToVList r1 ++ resultToVList r2

resultToArrays :: Arrays a => Result a ->  EmitArbb a
resultToArrays res = doResultToArray arrays res
  where 
    doResultToArray :: ArraysR arrs -> Result arrs -> EmitArbb arrs 
    doResultToArray ArraysRunit       ResultUnit   = return () 
    doResultToArray ArraysRarray      (ResultArray (InternalArray sh v)) = do
      ad <- varsToAD AD.arrayElt v 
      return $ Array sh ad -- (varsToAD AD.arrayElt v)
       
 
varsToAD :: ArrayEltR e -> Vars -> EmitArbb (AD.ArrayData e)
varsToAD ArrayEltRunit VarsUnit = return AD.AD_Unit
varsToAD (ArrayEltRpair r1 r2) (VarsPair a b) = do 
    a1 <- varsToAD r1 a 
    b1 <- varsToAD r2 b 
    return $ AD.AD_Pair a1 b1 -- (varsToAD r1 a) (varsToAD r2 b)
-- TODO: Change to the general case. use CPP hackery 
varsToAD (ArrayEltRint32) (VarsPrim v) = do
    primInt32 v 

-- TODO: WHAT IS THE RIGHT THING TO DO HERE ? 
-- WARNING: AREA OF "HACKING WITHOUT A CLUE" ! 
primInt32 :: forall a e. (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a) => 
             Variable -> EmitArbb (AD.ArrayData e)
primInt32 v = liftIO$ unsafeSTToIO$ do
    new <- AD.newArrayData 10  -- should depend on length
    -- TODO: Result should be copied from ArBB into this array
    AD.unsafeFreezeArrayData new
-- TODO: Move to Data.hs and make use of similar CPP hackery 
    
------------------------------------------------------------------------------ 
-- LookupArrayR  
lookupArrayR :: AD.ArrayElt e => 
               AD.ArrayData e ->  
               GlobalBindings Variable -> 
               Vars 
lookupArrayR ad gv = doLookup AD.arrayElt ad gv
  where 
    doLookup :: ArrayEltR e -> 
                AD.ArrayData e ->      
                GlobalBindings Variable -> 
                Vars 
    doLookup ArrayEltRunit             _  gv = VarsUnit
    doLookup (ArrayEltRpair aeR1 aeR2) ad gv =  
       let v1 = doLookup aeR1 (fst' ad) gv
           v2 = doLookup aeR2 (snd' ad) gv
       in VarsPair v1 v2
    doLookup aer                       ad gv = doLookupPrim aer ad gv
     where 
      { doLookupPrim :: ArrayEltR e -> AD.ArrayData e -> GlobalBindings Variable -> Vars
      mkPrimDispatch(doLookupPrim,lookupArrayRPrim)
      }

lookupArrayRPrim :: forall a e. (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a) => 
                 ScalarType -> 
                 AD.ArrayData e -> 
                 GlobalBindings Variable ->
                 Vars 
lookupArrayRPrim st ad  gv = 
   --let ptr = wordPtrToPtr (getArray ad)
   let ptr = getArray ad
   in case M.lookup  ptr gv  of 
        (Just v) -> VarsPrim v
        Nothing -> error "LookupArrayPrim: Implementation of ArBB backend is faulty!" 
