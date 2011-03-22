{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Data.Array.Accelerate.ArBB.Data2 where 

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
import           Intel.ArbbVM.Convenience hiding (liftIO)
import qualified Intel.ArbbVM.Type as ArBB 


import Foreign.Marshal.Array
import Foreign.Marshal.Utils

import Control.Monad.ST
import qualified Control.Monad.State.Strict as S 

import Data.Array.Accelerate.ArBB.State

import qualified Data.Map as M 
------------------------------------------------------------------------------
-- 

-- Array to name bindigs

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
-- 

{- 
data InternalArray sh where 
     InternalArray :: (Sugar.Shape sh) 
                   => Sugar.EltRepr sh 
                   -> Vars             -- bound to ArBB Variables 
                   -> InternalArray sh
-}
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
{- 
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
                
 -}
------------------------------------------------------------------------------ 
-- LookupArray that uses the state 

-- TODO: THIS IS HACKY, find better way 
lookupArray :: AD.ArrayElt e => 
               AD.ArrayData e ->  
  --             GlobalBindings Variable -> 
               ExecState (Maybe Vars)
lookupArray ad = doLookup AD.arrayElt ad {- gv -}
  where 
    doLookup :: ArrayEltR e -> 
                AD.ArrayData e ->      
--                GlobalBindings Variable -> 
                ExecState (Maybe Vars)
    doLookup ArrayEltRunit             _  = return$ Just VarsUnit
    doLookup (ArrayEltRpair aeR1 aeR2) ad =  do 
       v1 <- doLookup aeR1 (fst' ad) 
       v2 <- doLookup aeR2 (snd' ad) 
       case (v1,v2) of 
         (Just v1', Just v2') -> return$ Just (VarsPair v1' v2') 
         (Nothing,Nothing) -> return$ Nothing 
         (Just VarsUnit,Nothing) -> return$ Nothing -- hackity
         a -> error (show a) "lookupArray, Bug in ArBB backend" 
       
    doLookup aer                       ad = doLookupPrim aer ad
     where 
      { doLookupPrim :: ArrayEltR e -> AD.ArrayData e -> ExecState (Maybe Vars)
      mkPrimDispatch(doLookupPrim,lookupArrayPrim)
      }

lookupArrayPrim :: forall a e. (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a) => 
                 ScalarType -> 
                 AD.ArrayData e -> 
                 ExecState (Maybe Vars) 
lookupArrayPrim _ ad = do 
   arraymap <- S.get 
   
   let ptr = getArray ad -- get the key into the map
   liftIO$ putStrLn (show ptr)
   liftIO$ putStrLn (show arraymap)
   case M.lookup  ptr arraymap  of 
        (Just v) -> return$ Just (VarsPrim v)
        Nothing -> return Nothing -- error "LookupArrayPrim: Implementation of ArBB backend is faulty!" 



------------------------------------------------------------------------------
--
{- 
   Not sure how to do this but.. 
   Intermediate arrays are just "variables" in ArBB. 
   I dont actually need to manually allocate them, rather ArBB does that 
   for me!. 
   
   newArray is the ArBB part of the operation. 
   this should, when used, be paired up with th creation 
   of a new haskell (the one passed as key into this newArray function)

-} 


-- What do you really need for a new array, Type and Size! 
newArray :: (AD.ArrayElt e) => 
               AD.ArrayData e ->  
               Int -> -- Dimensions  
  --             GlobalBindings Variable -> 
               ExecState () 
newArray ad dims = doNew AD.arrayElt ad 
  where 
    doNew :: ArrayEltR e -> 
                AD.ArrayData e ->      
--                GlobalBindings Variable -> 
                ExecState () 
    doNew ArrayEltRunit             _  = return () -- VarsUnit
    doNew (ArrayEltRpair aeR1 aeR2) ad =  do 
       {-v1 <- -} doNew aeR1 (fst' ad) 
       {-v2 <- -} doNew aeR2 (snd' ad) 
       -- return$ VarsPair v1 v2
    doNew aer                       ad = doNewPrim aer dims ad -- dims 
     where 
      { doNewPrim :: ArrayEltR e -> Int -> AD.ArrayData e -> ExecState ()
      mkPrimDispatch(doNewPrim,newArrayPrim)
      }


-- TODO: Size may not be needed here, investigate. 
newArrayPrim :: forall a e. (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a) => 
                 ScalarType -> -- type of elements, and num of dimensions
                 Int ->  -- dimensions          
                 AD.ArrayData e -> -- key into table 
                 ExecState () 
newArrayPrim st d ad = do 
   arraymap <- S.get 
   
   let ptr = getArray ad -- get the key into the map
   case M.lookup  ptr arraymap  of 
        (Just _) -> error "newArrayPrim: Implementation of ArBB backend is faulty!" 
        
        -- The answer should be NO right ? otherwise someone already tried 
        -- to "create" this array 
        Nothing -> do 
          t <- liftArBB$  getScalarType_ st
          dt <- liftArBB$ getDenseType_ t d -- get n dimensional (up to three) 
          bin <- liftArBB$ getBindingNull_ 
          g <- liftArBB$ createGlobal_ dt "Optimus_Prime" bin
          v <- liftArBB$ variableFromGlobal_ g
          S.put (M.insert ptr v arraymap) 
          return ()




------------------------------------------------------------------------------
-- RENAME THIS COPYIN
-- Create arrays on ArBB side and load the data ! 
copyIn :: (AD.ArrayElt e) 
       => AD.ArrayData e 
     --  -> Int  -- Total size 
       -> [Int] -- dimensions
       -> ExecState () 
copyIn ad d = doCopyIn AD.arrayElt ad 
  where 
    doCopyIn :: ArrayEltR e -> 
                AD.ArrayData e ->      
--                GlobalBindings Variable -> 
                ExecState () 
    doCopyIn ArrayEltRunit             _  = return () -- VarsUnit
    doCopyIn (ArrayEltRpair aeR1 aeR2) ad =  do 
       {-v1 <- -} doCopyIn aeR1 (fst' ad) 
       {-v2 <- -} doCopyIn aeR2 (snd' ad) 
       -- return$ VarsPair v1 v2
    doCopyIn aer                       ad = doCopyInPrim aer d ad -- dims 
     where 
      { doCopyInPrim :: ArrayEltR e -> [Int] -> AD.ArrayData e -> ExecState ()
      mkPrimDispatch(doCopyInPrim,copyInPrim)
      }


-- TODO: Size may not be needed here, investigate. 
copyInPrim :: forall a e. (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a) => 
                 ScalarType -> -- type of elements, and num of dimensions
                 --Int ->  -- total size 
                 [Int] ->  -- dimensions          
                 AD.ArrayData e -> -- key into table 
                 ExecState () 
copyInPrim st d ad = do 
   arraymap <- S.get 
   
   let ptr = getArray ad -- get the key into the map
   case M.lookup  ptr arraymap  of 
        (Just _) -> error "newArrayPrim: Implementation of ArBB backend is faulty!" 
        
        -- The answer should be NO right ? otherwise someone already tried 
        -- to "create" this array 
        Nothing -> do 
          
          t <- liftArBB$  getScalarType_ st
          dt <- liftArBB$ getDenseType_ t (length d) -- get n dimensional (up to three) 
          bin <- liftArBB$ getBindingNull_ 
          g <- liftArBB$ createGlobal_ dt "Optimus_Prime" bin
          v <- liftArBB$ variableFromGlobal_ g
          
        
-- Allocate something of size s         
          liftIO$ putStrLn "call ArbbOpAlloc" 
          num_elems <- liftArBB$ mapM (usize_ ) d 
          let s = foldr (+) 0 d
          
          liftArBB$ opDynamicImm_ ArbbOpAlloc [v] num_elems 
          liftIO$ putStrLn "call ArbbOpAlloc DONE" 
          m_ptr <- liftArBB$ mapToHost_ v [1] ArbbWriteOnlyRange --whats the one ?
          liftIO$ copyBytes m_ptr ptr ((fromIntegral s) * (ArBB.size st))                   
-- --       
          S.put (M.insert ptr v arraymap) 
          return ()



------------------------------------------------------------------------------
-- COPYOUT 
copyOut :: (AD.ArrayElt e) => 
               AD.ArrayData e ->  
               Int -> -- Total size 
               ExecState () 
copyOut ad s = doCopyOut AD.arrayElt ad 
  where 
    doCopyOut :: ArrayEltR e -> 
                AD.ArrayData e ->      
--                GlobalBindings Variable -> 
                ExecState () 
    doCopyOut ArrayEltRunit             _  = return () -- VarsUnit
    doCopyOut (ArrayEltRpair aeR1 aeR2) ad =  do 
       {-v1 <- -} doCopyOut aeR1 (fst' ad) 
       {-v2 <- -} doCopyOut aeR2 (snd' ad) 
       -- return$ VarsPair v1 v2
    doCopyOut aer                       ad = doCopyOutPrim aer s ad -- dims 
     where 
      { doCopyOutPrim :: ArrayEltR e -> Int -> AD.ArrayData e -> ExecState ()
      mkPrimDispatch(doCopyOutPrim,copyOutPrim)
      }


-- TODO: Size may not be needed here, investigate. 
copyOutPrim :: forall a e. (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a) => 
                 ScalarType -> -- type of elements, and num of dimensions
                 Int ->  -- total size 
                 AD.ArrayData e -> -- key into table 
                 ExecState () 
copyOutPrim st s ad = do 
   arraymap <- S.get 
   
   let ptr = getArray ad -- get the key into the map
   case M.lookup ptr arraymap of 
        (Just v) -> do
          m_ptr <- liftArBB$ mapToHost_ v [1 {-pitch-}] ArbbWriteOnlyRange 
          num_elems <- liftArBB$ usize_ s 
          liftIO$ copyBytes ptr m_ptr ((fromIntegral s) * (ArBB.size st))                   
          return ()
        Nothing -> error "ArBB back-end is buggy"
