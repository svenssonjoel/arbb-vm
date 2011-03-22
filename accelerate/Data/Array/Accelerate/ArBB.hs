{-# LANGUAGE GADTs, RankNTypes #-} 
{-# LANGUAGE FlexibleInstances, PatternGuards, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}


------------------------------------------------------------------------------
{- 
 This file contains an ArBB D.A.Accelerate backend that uses 
 ArBB in an "immediate mode". Small ArBB functions are generated and executed 
 interleaved with xImm_ calls for various tasks. 
-}


module Data.Array.Accelerate.ArBB where 

import Intel.ArbbVM
import Intel.ArbbVM.Convenience hiding (liftIO)
import qualified Intel.ArbbVM.Convenience as ArBB

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Type as Type

import Data.Array.Accelerate.Array.Sugar  (Array(..), Segments, eltType)
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import           Data.Array.Accelerate.Array.Sugar ((:.)(..))
import qualified Data.Array.Accelerate.Smart       as Sugar
import qualified Data.Array.Accelerate.Array.Data as AD 

import Data.Array.Accelerate.Array.Representation

import Data.Array.Accelerate.Analysis.Type

import Foreign.Ptr 
import Foreign.Marshal.Array hiding (newArray) 


-- WHEN Data2 works change name to Data
import Data.Array.Accelerate.ArBB.Data
import Data.Array.Accelerate.ArBB.Type
import Data.Array.Accelerate.ArBB.Gen
import Data.Array.Accelerate.ArBB.State

import Data.Typeable
import Data.Int
import Data.Maybe

import qualified Data.Map as M

import System.IO.Unsafe

import qualified  Control.Monad.State.Strict as S 
import Control.Monad
import Control.Applicative
#define L S.lift$

{- 

   Attempt no 3 (or 4) at an ArBB back-end for Accelerate. 
    This time we are much more "copying" the approach of the 
    CUDA backend. 
    Each "real" arrays has a "shadow" array in Haskell world.
     The "real" arrays are managed by ArBB 
       - This is of course very wasteful! 
       + So far, it works.. 

    
     GLOBAL_TODO_LIST:
       # Add reference counting to the arrays (look at what CUDA back-end does) 
   


     I think people often say that the functional programming paradigm
       makes it easier to write correct programs (or some-such ?) 
       
     For me I think FP makes it harder to write bad programs. 
           (And often what I want to write, are bad programs!) 
          
-}
    
------------------------------------------------------------------------------
-- run (The entry point) 

run :: Arrays a => Sugar.Acc a -> a 
run acc = unsafePerformIO$ arbbSession$ do 
     let acc' = (Sugar.convertAcc acc)
     (a,_) <- runExecState (collect =<< executeArBB acc' Empty) 
     return a

------------------------------------------------------------------------------ 
-- collect (98% Copy-n-paste from CUDA back-end) 
collect :: Arrays a
        =>  a
        -> ExecState a
collect a = collectR arrays a
  where 
    collectR :: ArraysR a -> a -> ExecState a   
    collectR ArraysRunit  () = return ()
    collectR ArraysRarray  (arr@(Array sh ad)) =  do
         copyOut ad (size sh)
         return arr 
    collectR (ArraysRpair ar1 ar2) (a1,a2) = do 
       (,) <$> collectR ar1 a1 <*> collectR ar2 a2 



------------------------------------------------------------------------------
-- ExecuteArBB 
executeArBB :: (Typeable aenv, Typeable a ) =>  
               OpenAcc aenv a -> 
               Val aenv ->  
               ExecState a -- ()  
executeArBB acc@(OpenAcc pacc) aenv = do
   case pacc of 
   
      Let a b ->  do 
        a0 <- executeArBB a aenv   
        executeArBB b (aenv `Push` a0) 
  
      Let2 a b -> do 
        (a1,a0) <- executeArBB a aenv 
        executeArBB b (aenv `Push` a1 `Push` a0)  

      Avar ix -> return$ prj ix aenv 
        
      Use a -> useOp a -- load array here 

      Map _ a -> do
        a0 <- executeArBB a aenv 
        mapOp acc aenv a0 -- could pass just "f" and a0 ? 
      
      ZipWith _ a b -> do 
        a0 <- executeArBB a aenv
        b0 <- executeArBB b aenv 
        zipWithOp acc aenv a0 b0 -- could pass just "f" and a0 + b0 ?
      
      -- TODO: Implement same as Fold1
      Fold x y a  -> do -- error "Fold: Not yet implemented"
        a0 <- executeArBB a aenv
        fold1Op (OpenAcc (Fold1 x a)) aenv a0  -- Major cheat 
      -- TODO: Implement!
      Fold1 _ a -> do 
        a0 <- executeArBB a aenv 
        fold1Op acc aenv a0 
      
      FoldSeg _ _ _ _ ->  error "FoldSeg: Not yet implemented" 
      
      Fold1Seg _ _ _ ->  error "Fold1Seg: Not yet implemented" 
      
      -- GO on with Scans, Permutes, Stencils 
      
        
------------------------------------------------------------------------------
-- USE 

useOp :: Array dim e
      -> ExecState (Array dim e) 
useOp inp@(Array sh ad)  | dim sh <= 3 = do 
  res <- lookupArray ad
  case res of 
       Just v -> return  inp -- This can happen if Let is used 
       Nothing -> do
          liftIO$ putStrLn "Creating new ArBB Array" 
          liftIO$ putStrLn (show d)
          copyIn ad d -- allocates on Arbb Side 
          liftIO$ putStrLn "Creating new ArBB Array DONE" 
          return$ inp -- Array (sh) ad
        where
           --n = size sh
           d = shapeToList sh
useOp _ = error "Use: ArBB back-end does not support arrays of dimension higher than 3"
      
------------------------------------------------------------------------------
-- mapOp 

mapOp :: (Sugar.Elt e, Typeable aenv)
      => OpenAcc aenv (Array dim e)
      -> Val aenv 
      -> Array dim e'
      -> ExecState (Array dim e)
mapOp acc@(OpenAcc (Map f inp))  aenv (Array sh0 in0) | dim sh0 <= 3 = do  
  inputArray' <- lookupArray in0 -- find the input variables
  
  outputArray <- ad `seq` newArray ad d

  let inputArray = fromJust inputArray'  -- HACKITY
      ot = getAccType' acc
      it = getAccType' inp   


  fun <- genMap ot -- output type (of elements)
                it -- input type (of elemets)  
                f 
  
  out_dense' <- defineDenseTypesNew ot  d
  inp_dense' <- defineDenseTypesNew it  d
  
  let inp_vars = varsToList inputArray
      out_vars = varsToList outputArray 
      inp_dense = arBBTypeToList inp_dense'
      out_dense = arBBTypeToList out_dense' 

  maper <- liftArBB$ funDef_ "aap" out_dense inp_dense $ \ out inp -> do
    map_ fun out inp

 ----------
  str <- liftArBB$ serializeFunction_ maper 
  liftIO$ putStrLn (getCString str)
 ---------    

  liftArBB$ execute_ maper out_vars inp_vars  
          
  return$ Array (sh0) ad
  where
    n = size sh0
    d = dim sh0
    (ad,_) = AD.runArrayData $ (,undefined) `fmap` AD.newArrayData (1024 `max` n)

mapOp _ _ _ = 
  error "Map: ArBB back-end does not support arrays of dimension higher than 3"



------------------------------------------------------------------------------
--

zipWithOp :: (Sugar.Elt e, Typeable aenv)
          => OpenAcc aenv (Array dim e)
          -> Val aenv
          -> Array dim e1
          -> Array dim e2
          -> ExecState (Array dim e)
zipWithOp acc@(OpenAcc (ZipWith f inp0 inp1))
          aenv 
          (Array sh0 in0)  
          (Array sh1 in1) = do  
  inputArray0' <- lookupArray in0 -- find the input variables
  inputArray1' <- lookupArray in1  

  outputArray <- ad `seq` newArray ad d  -- create array 
 
  let inputArray0 = fromJust inputArray0' -- HACKITY
      inputArray1 = fromJust inputArray1' -- HACKITY
      ot = getAccType' acc
      it0 = getAccType' inp0
      it1 = getAccType' inp1

  fun <- genBFun ot  -- output type (of elements)
                 it0 -- input type (of elements)  
                 it1 -- input type (of elements) 
                 f 
                 
  -- all three same dimensionality
  out_dense'  <- defineDenseTypesNew ot  d
  inp_dense0' <- defineDenseTypesNew it0 d
  inp_dense1' <- defineDenseTypesNew it1 d

  let inp_vars = varsToList inputArray0 ++ varsToList inputArray1 
      out_vars = varsToList outputArray 
      inp_dense = arBBTypeToList inp_dense0' ++ arBBTypeToList inp_dense1'
      out_dense = arBBTypeToList out_dense' 

  zipper <- liftArBB$ funDef_ "aap" out_dense inp_dense $ \ out inp -> do
    map_ fun out inp

 ----------
  str <- liftArBB$ serializeFunction_ zipper 
  -- liftIO$ putStrLn "mapee function" 
  liftIO$ putStrLn (getCString str)
 ---------    

  liftArBB$ execute_ zipper out_vars inp_vars  
          
  return$ Array sh0 ad
  where
    n = size sh0
    d = dim sh0
    (ad,_) = AD.runArrayData $ (,undefined) `fmap` AD.newArrayData (1024 `max` n)
     

------------------------------------------------------------------------------
-- Fold1Op 

--TODO: in the one to zero dimensions case, use ArbbOpConstVector 
--      To create one element array.(hopefully I wont need new code 
--      in Data.hs with this approach.) 

fold1Op :: (Sugar.Elt e, Typeable aenv)
        => OpenAcc aenv (Array sh e)
        -> Val aenv
        -> Array (sh:.Int) e'
        -> ExecState (Array sh e)
fold1Op acc@(OpenAcc (Fold1 f@(Lam (Lam (Body (PrimApp op _))))  inp0)) -- POSSIBLY SIMPLY CASE
        aenv
        in0  = do
  primFold op in0  

  return$ error "N/A"
fold1Op _ _ _ = error "Fold1Op: N/A" -- THE TRICKY CASE 
   -- TODO: Implement using "handwritten" ArBB reductions 


-- TODO: Why does it need to be this ugly. 
--       Why can I not simply have a case statement in the fold1Op function above?
--       the complaint is "GADT pattern match with non-rigid result type"
primFold :: PrimFun (t -> t1) -> Array (sh:.Int) e -> ExecState ()
primFold (PrimAdd _) in0 =
  liftIO$ putStrLn "GOTO ReduceAdd"
primFold (PrimSub _) in0 =  
  liftIO$ putStrLn "GOTO ReduceSub"
-- TODO: And so on!
  



------------------------------------------------------------------------------
-- This is function definition.. genMap is bad name !! 
-- TODO: Generalise function generation.

genMap :: ArBBType ScalarType -> 
          ArBBType ScalarType -> 
          OpenFun env aenv t -> 
          ExecState Function 
genMap out inp fun = do
  out' <- defineTypesNew out
  inp' <- defineTypesNew inp

  let l_inp = arBBTypeToList inp' 
  let l_out = arBBTypeToList out'
  -- Start by generating the function to be mapped!
  fun <- liftArBB$ funDef_ "f" l_out l_inp $ \ outs inps -> do 
    vars <- genFun fun inps -- inputs as the "environment"  
    zipWithM_ copy_ outs vars
   
----------
  str <- liftArBB$ serializeFunction_ fun 
  -- liftIO$ putStrLn "mapee function" 
  liftIO$ putStrLn (getCString str)
---------  
    
  return fun


genBFun :: ArBBType ScalarType -> 
           ArBBType ScalarType ->
           ArBBType ScalarType ->  
           OpenFun env aenv t -> 
           ExecState Function 
genBFun out inp1 inp2 fun = do
  out' <- defineTypesNew out
  inp1' <- defineTypesNew inp1
  inp2' <- defineTypesNew inp2

  let l_inp = arBBTypeToList inp1' ++ arBBTypeToList inp2'
  let l_out = arBBTypeToList out'
  -- Start by generating the function to be mapped!
  fun <- liftArBB$ funDef_ "f" l_out l_inp $ \ outs inps -> do 
    vars <- genFun fun inps -- inputs as the "environment"  
    zipWithM_ copy_ outs vars
    --assignTo outs vars -- Working with lists here ! (keep track of where to do what!) 
----------
  str <- liftArBB$ serializeFunction_ fun 
  -- liftIO$ putStrLn "mapee function" 
  liftIO$ putStrLn (getCString str)
---------  
    
  return fun

 
------------------------------------------------------------------------------
-- Assign 
assignToVars VarsUnit VarsUnit = return ()
assignToVars (VarsPrim v1) (VarsPrim v2) = do 
   op_ ArbbOpCopy [v1] [v2]
assignToVars (VarsPair v1 v2) ( VarsPair v3 v4) = do 
   assignToVars v1 v3
   assignToVars v2 v4 
assignToVars x y = error "assignToVars: Mismatch" 

assignToVarsImm VarsUnit VarsUnit = return ()
assignToVarsImm (VarsPrim v1) (VarsPrim v2) = do 
   opImm_ ArbbOpCopy [v1] [v2]
assignToVarsImm (VarsPair v1 v2) ( VarsPair v3 v4) = do 
   assignToVarsImm v1 v3
   assignToVarsImm v2 v4 
assignToVarsImm x y = error "assignToVars: Mismatch" 

------------------------------------------------------------------------------
-- define ArBB VM types for a list of type "names" 
defineTypes :: [ScalarType] -> EmitArbb [Type]
defineTypes [] = return []
defineTypes (x:xs) = do 
   t <- getScalarType_ x 
   ts <- defineTypes xs 
   return (t:ts)

defineDenseTypes :: [ScalarType] -> Int -> EmitArbb [Type] 
defineDenseTypes [] _ = return []
defineDenseTypes (x:xs) dims = do 
   t <- getScalarType_ x
   d <- getDenseType_ t dims 
   ds <- defineDenseTypes xs dims 
   return (d:ds)
 

defineLocalVars :: [Type] -> EmitArbb [Variable]
defineLocalVars [] = return [] 
defineLocalVars (t:ts) = do 
  let name = "name"
  v <- createLocal_ t name -- "name" -- name needs to be unique ? 
  vs <- defineLocalVars ts
  return (v:vs) 

{-
defineGlobalVars :: [Type] -> EmitArbb [Variable]
defineGlobalVars [] = return [] 
defineGlobalVars (t:ts) = do 
  let name = "name"
  -- liftIO$ putStrLn ("Creating local variable: " ++name)    
  v <- createGlobal_ t name -- "name" -- name needs to be unique ? 
  vs <- defineGlobalVars ts
  return (v:vs) 
-}

defineTypesNew :: ArBBType ScalarType -> ExecState (ArBBType Type)
defineTypesNew ArBBTypeUnit = return ArBBTypeUnit
defineTypesNew (ArBBTypeSingle st)  = do 
   t <- liftArBB$ getScalarType_ st 
   return$ ArBBTypeSingle t
defineTypesNew (ArBBTypePair st1 st2) = do 
   t1 <- defineTypesNew st1
   t2 <- defineTypesNew st2 
   return$ ArBBTypePair t1 t2

defineDenseTypesNew :: ArBBType ScalarType -> Int -> ExecState (ArBBType Type) 
defineDenseTypesNew ArBBTypeUnit _ = return ArBBTypeUnit
defineDenseTypesNew (ArBBTypeSingle st) dims = do 
  t <- liftArBB$ getScalarType_ st
  d <- liftArBB$ getDenseType_ t dims
  return (ArBBTypeSingle d) 
defineDenseTypesNew (ArBBTypePair st1 st2) dims = do 
  t1 <- defineDenseTypesNew st1 dims
  t2 <- defineDenseTypesNew st2 dims
  return (ArBBTypePair t1 t2) 



defineLocalVarsNew :: ArBBType Type -> ExecState Vars
defineLocalVarsNew ArBBTypeUnit = return VarsUnit
defineLocalVarsNew (ArBBTypeSingle t) = do 
  let name = "name"
  -- liftIO$ putStrLn ("Creating local variable: " ++name)    
  v <- liftArBB$ createLocal_ t name -- "name" -- name needs to be unique ? 
  return$ VarsPrim v
defineLocalVarsNew (ArBBTypePair t1 t2) = do 
  v1 <- defineLocalVarsNew t1
  v2 <- defineLocalVarsNew t2 
  return$ VarsPair v1 v2

defineGlobalVarsNew :: ArBBType Type -> ExecState Vars
defineGlobalVarsNew ArBBTypeUnit = return VarsUnit
defineGlobalVarsNew (ArBBTypeSingle t) = do 
  let name = "name"
  b <- liftArBB$ getBindingNull_
  gv <- liftArBB$ createGlobal_ t name b -- "name" -- name needs to be unique ? 
  v  <- liftArBB$ variableFromGlobal_ gv
  return$ VarsPrim v
defineGlobalVarsNew (ArBBTypePair t1 t2) = do 
  v1 <- defineGlobalVarsNew t1
  v2 <- defineGlobalVarsNew t2 
  return$ VarsPair v1 v2
