{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Data.Array.Accelerate.ArBB.Data where 

import Foreign.Ptr
import Data.Int
import Data.Word
import Data.Typeable
--import Data.Maybe
--import Data.Word

import qualified Data.Array.Accelerate.Array.Data as AD
import           Data.Array.Accelerate.Array.Data (ArrayEltR(..)) 
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import           Data.Array.Accelerate.Array.Sugar (Array(..))
import           Data.Array.Accelerate.AST
import Data.Array.Accelerate.ArBB.Type
import Data.Array.Accelerate.Array.Representation

import Intel.ArbbVM
import Intel.ArbbVM.Convenience

-- for debug
import Foreign.Marshal.Array

-- Use to carry along a Map of bindings
import qualified Control.Monad.State as ST

import qualified Data.Map as M 
------------------------------------------------------------------------------
-- 

-- Array to name bindigs
type GlobalBindings a = M.Map (Ptr ()) a 

data ArrayDesc = ArrayDesc { arrayDescLength :: Int, 
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
-- bindArray 
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
-- Collect global bindings

collectGlobals :: (Typeable aenv, Typeable a) =>        
                  OpenAcc aenv a -> 
                  GlobalBindings ArrayDesc ->  
                  GlobalBindings ArrayDesc
collectGlobals acc@(OpenAcc pacc) gb =
     case pacc of 
       (Use (Array sh ad)) -> 
         let n = size sh 
         in insertArray ad n gb  
       (Map f acc) -> collectGlobals acc gb 

------------------------------------------------------------------------------
-- insertArray

insertArray :: AD.ArrayElt e => 
               AD.ArrayData e -> Int -> 
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
      { doInsertPrim :: ArrayEltR e -> AD.ArrayData e -> Int -> GlobalBindings ArrayDesc -> GlobalBindings ArrayDesc
      mkPrimDispatch(doInsertPrim,insertArrayPrim)
      }

insertArrayPrim :: forall a e. (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a) => 
                 ScalarType -> 
                 AD.ArrayData e -> 
                 Int -> GlobalBindings ArrayDesc ->
                 GlobalBindings ArrayDesc 
insertArrayPrim st ad n gb = 
   let ptr = wordPtrToPtr (getArray ad)
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
   let ptr = wordPtrToPtr (getArray ad)
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
       liftIO$ putStrLn (show n)
       liftIO$ putStrLn (show st)
       array <- liftIO$ peekArray 10 (castPtr ptr)  
       liftIO$ putStrLn (show (array :: [Int32]))    
       
-- ArBB CALLS           
       bin <- createDenseBinding_ ptr 1 [fromIntegral n] [4 {- sizeof element -}]
       liftIO$ putStrLn ("Binding: " ++ show ptr)
       liftIO$ putStrLn ("   To variable: input" ++ show ptr)
       sty <- getScalarType_ st   
       dty <- getDenseType_ sty 1  
       gin <- createGlobal_ dty ("input") bin 
       v <- variableFromGlobal_ gin
-------------
       
       gv' <- doBindGlobals xs gv
       let gvout = M.insert ptr v gv'  -- return [v]
       return gvout
  
            