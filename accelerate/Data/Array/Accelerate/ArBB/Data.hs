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
       (ZipWith _ a1 a2) -> let gb' = collectGlobals a1 gb
                            in  collectGlobals a2 gb'

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
--       liftIO$ putStrLn (show n)
--       liftIO$ putStrLn (show st)
--       array <- liftIO$ peekArray 10 (castPtr ptr)  
--       liftIO$ putStrLn (show (array :: [Int32]))    
       
-- ArBB CALLS 

       let name = "input" ++ show ptr
       liftIO$ putStrLn ("   To variable: "  ++ name)
      
       bin <- getBindingNull_
       sty <- getScalarType_ st   
       dty <- getDenseType_ sty 1  
       gin <- createGlobal_ dty name bin 

       v <- variableFromGlobal_ gin
       
       num_elems <- usize_ n 
        
       -- this executes immediately (allocates space for num_elements
       -- on the ArBB "side") 
       opDynamicImm_ ArbbOpAlloc [v] [num_elems]
     
       -- mapToHost space and copy bytes into ArBB 
       m_ptr <- mapToHost_ v [1] ArbbReadWriteRange
       liftIO$ copyBytes m_ptr ptr ((fromIntegral n) * (ArBB.size st))                
-------------
       
       gv' <- doBindGlobals xs gv
       let gvout = M.insert ptr v gv' 
       liftIO$ putStrLn "leaving bindGlobals" 
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

-- preorder traversal 
varsToList VarsUnit =[] 
varsToList (VarsPrim v)  = [v] 
varsToList (VarsPair v1 v2) = varsToList v1 ++ varsToList v2

listToVars v xs = let ([],a) =  doListToVars v xs in a 
  where 
   doListToVars VarsUnit xs = (xs, VarsUnit)
   doListToVars (VarsPrim _) (x:xs) = (xs, VarsPrim x)
   doListToVars (VarsPair v1 v2) xs = 
      let (rest, r1) = doListToVars v1 xs 
          (rest',r2) = doListToVars v2 rest 
      in (rest',VarsPair r1 r2)

------------------------------------------------------------------------------ 
-- Result is a structure of ArBB variables that mirrors 
-- the structure of "Arrays" in Accelerate
data Result arrs where 
  ResultUnit :: Result () 
  ResultArray :: (Sugar.Shape sh, Sugar.Elt e) => InternalArray sh -> Result (Array sh e) 
  ResultPair :: Result a1 -> Result a2 -> Result (a1,a2)

-- again preorder traversal
resultToVList :: Result  a -> [Vars] 
resultToVList ResultUnit = [] 
resultToVList (ResultArray (InternalArray _ vars)) = [vars] 
resultToVList (ResultPair r1 r2) = resultToVList r1 ++ resultToVList r2

-- resultToTList :: Result a -> [

------------------------------------------------------------------------------
-- resultToArrays 
resultToArrays :: Arrays a => Result a ->  EmitArbb a
resultToArrays res = doResultToArray arrays res
  where 
    doResultToArray :: ArraysR arrs -> Result arrs -> EmitArbb arrs 
    doResultToArray ArraysRunit       ResultUnit   = return () 
    doResultToArray ArraysRarray      (ResultArray (InternalArray sh v)) = do
      ad <- varsToAD AD.arrayElt v (size sh)
      return $ Array sh ad -- (varsToAD AD.arrayElt v)
     

  
------------------------------------------------------------------------------
--
varsToAD :: ArrayEltR e -> Vars -> Int -> EmitArbb (AD.ArrayData e)
varsToAD ArrayEltRunit VarsUnit  n = return AD.AD_Unit
varsToAD (ArrayEltRpair r1 r2) (VarsPair a b) n = do 
    a1 <- varsToAD r1 a n 
    b1 <- varsToAD r2 b n 
    return $ AD.AD_Pair a1 b1 -- (varsToAD r1 a) (varsToAD r2 b)
-- TODO: Change to the general case. use CPP hackery 
varsToAD (ArrayEltRint32) (VarsPrim v) n = do
    primInt32 v n
varsToAD (ArrayEltRint64) (VarsPrim v) n = do
    primInt32 v n
varsToAD (ArrayEltRint) (VarsPrim v) n = do
    primInt32 v n
varsToAD (ArrayEltRpair _ _) (VarsPrim _) _ = error "varsToAD: mismatch0"
varsToAD (ArrayEltRunit)     (VarsPrim _) _ = error "varsToAD: mismatch1"      
varsToAD _ _ _ = error "varsToAD: mismatch2"    

-- TODO: WHAT IS THE RIGHT THING TO DO HERE ? 
-- WARNING: AREA OF "HACKING WITHOUT A CLUE" ! 
primInt32 :: forall a e. (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a) => 
             Variable -> Int -> EmitArbb (AD.ArrayData e)
primInt32 v n = do
    ptr <- mapToHost_ v [1] ArbbReadOnlyRange
    dat <- liftIO$ peekArray n (castPtr ptr) :: EmitArbb [Int]
    -- liftIO$ putStrLn (show dat)
    liftIO$ unsafeSTToIO$ do
     new <- AD.newArrayData n  -- should depend on length
 
     targ <- AD.ptrsOfMutableArrayData new
     unsafeIOToST$ copyBytes (castPtr targ) ptr ( n * 4 {- sizeof Int32 -})
     AD.unsafeFreezeArrayData new
-- TODO:  make use of  CPP hackery 

outputVariables :: Result a -> EmitArbb (Result a) 
outputVariables ResultUnit = return ResultUnit 
outputVariables (ResultArray (InternalArray sh vars)) = do 
    vars' <- newOutputVars (size sh) vars 
    return (ResultArray (InternalArray sh vars'))
outputVariables (ResultPair  r1 r2) = do 
    r1' <- outputVariables r1
    r2' <- outputVariables r2 
    return (ResultPair r1' r2') 

newOutputVars :: Int -> Vars -> EmitArbb Vars
newOutputVars n (VarsUnit) = return VarsUnit
newOutputVars n (VarsPrim v) = do 
    bin <- getBindingNull_
    t <- getScalarType_ ArbbI32  -- HACK 
    dt <- getDenseType_ t 1      -- HACK
    gout <- createGlobal_ dt "out" bin
    v <- variableFromGlobal_ gout 
    
    num_elems <- usize_ n 
    opDynamicImm_ ArbbOpAlloc [v] [num_elems]
    return (VarsPrim v) 
newOutputVars n (VarsPair v1 v2) = do 
    v1' <- newOutputVars n v1     
    v2' <- newOutputVars n v2 
    return (VarsPair v1' v2')        
                
               
    
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
