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


module Data.Array.Accelerate.ArBBimm where 

import Intel.ArbbVM
import Intel.ArbbVM.Convenience hiding (liftIO)
import qualified Intel.ArbbVM.Convenience as ArBB

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Type as Type

import Data.Array.Accelerate.Array.Sugar  (Array(..), Segments, eltType)
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import qualified Data.Array.Accelerate.Smart       as Sugar
import qualified Data.Array.Accelerate.Array.Data as AD 

import Data.Array.Accelerate.Array.Representation

import Data.Array.Accelerate.Analysis.Type

import Foreign.Ptr 
import Foreign.Marshal.Array hiding (newArray) 


-- WHEN Data2 works change name to Data
import Data.Array.Accelerate.ArBB.Data2
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

      -- TODO: These need consider the "shape" of the input arrays also.
      Map _ a -> do
        a0 <- executeArBB a aenv 
        mapOp acc aenv a0 -- could pass just "f" and a0 ? 
      -- TODO: the function that is mapped onto the array 
      --         may use arrays bound in the environment. 
      --         That is why "aenv" must be passed to mapOp
     
      -- TODO: Implement zipWithOp 
      ZipWith _ a b -> do 
        a0 <- executeArBB a aenv
        b0 <- executeArBB b aenv 
        zipWithOp acc aenv a0 b0 -- could pass just "f" and a0 + b0 ?
           
     
  
{- 
     m@(Map f acc) -> execMap (getAccType' (OpenAcc m))  -- output type (of elements)
                              (getAccType'  acc)         -- input type (of elemets)  
                              f =<< executeArBB acc aenv gv 
     zw@(ZipWith f ac1 ac2) -> execZipWith (getAccType' (OpenAcc zw))
                                           (getAccType' ac1)
                                           (getAccType' ac2) 
                                           f =<< liftM2 (,) (executeArBB ac1 aenv gv) 
                                                            (executeArBB ac2 aenv gv)
     fd@(Fold f e ac1) -> execFold (getAccType (OpenAcc fd)) 
                                   (getExpType e) 
                                   (getAccType ac1) =<< executeArBB ac1 aenv gv
     
    --resultToArrays =<< executeArBB' acc aenv glob_vars
-} 


------------------------------------------------------------------------------
-- USE 

useOp :: Array dim e
      -> ExecState (Array dim e) 
useOp inp@(Array sh ad)  = do 
  res <- lookupArray ad
  case res of 
       Just v -> return  inp -- This can happen if Let is used 
       Nothing -> do
          liftIO$ putStrLn "Creating new ArBB Array" 
          copyIn ad d -- allocates on Arbb Side 
          liftIO$ putStrLn "Creating new ArBB Array DONE" 
          return$ inp -- Array (sh) ad
        where
           --n = size sh
           d = shapeToList sh
      
------------------------------------------------------------------------------
-- mapOp below SEEMS to work. 
--  TODO: Clean-up crew is definitely needed! 
--  TODO: (ArBB)Typechecking fails when input array is not 1D 

mapOp :: (Sugar.Elt e, Typeable aenv)
      => OpenAcc aenv (Array dim e)
      -> Val aenv -- is it used ? (CLEAN UP HERE !! ) 
      -> Array dim e'
      -> ExecState (Array dim e)
mapOp acc@(OpenAcc (Map f inp))  aenv (Array sh0 in0)  = do  
  inputArray' <- lookupArray in0 -- find the input variables
  ad `seq` newArray ad d
  vs' <- lookupArray ad -- find the output variables
  

  -- Compute map f -----
  -- input variables are in inputArray
  -- outputs sould be placed in "vs"   (Improve names)          
  let vs = fromJust vs' -- HACKITY 
  let inputArray = fromJust inputArray' -- HACKITY
  let ot = getAccType' acc
  let it = getAccType' inp   

  liftIO$ putStrLn "Generating mapee" 
  fun <- genMap ot -- output type (of elements)
                it -- input type (of elemets)  
                f 
  liftIO$ putStrLn "Generating mapee DONE" 
                
   
  out_dense' <- defineDenseTypesNew ot  d
  inp_dense' <- defineDenseTypesNew it  d
  --out_vars' <- defineGlobalVarsNew out_dense'
  --inp_vars' <- defineGlobalVarsNew inp_dense'
  
  --assignToVarsImm inp_vars' v
  
  let inp_vars = varsToList inputArray
  let out_vars = varsToList vs 
  let inp_dense = arBBTypeToList inp_dense'
  let out_dense = arBBTypeToList out_dense' 

  maper <- liftArBB$ funDef_ "aap" out_dense inp_dense $ \ out inp -> do
    map_ fun out inp

 ----------
  str <- liftArBB$ serializeFunction_ maper 
  -- liftIO$ putStrLn "mapee function" 
  liftIO$ putStrLn (getCString str)
 ---------    

  liftArBB$ execute_ maper out_vars inp_vars  
  ----------------------
          
  return$ Array (sh0) ad
  where
    n = size sh0
    d = dim sh0
    (ad,_) = AD.runArrayData $ (,undefined) `fmap` AD.newArrayData (1024 `max` n)
   


------------------------------------------------------------------------------
--

zipWithOp :: (Sugar.Elt e, Typeable aenv)
          => OpenAcc aenv (Array dim e)
          -> Val aenv
          -> Array dim e1
          -> Array dim e2
          -> ExecState (Array dim e)
zipWithOp acc@(OpenAcc (ZipWith f inp0 inp1)) --   
          aenv -- will i use it ?  
          (Array sh0 in0)   -- same as inp0 !!! redundant info
          (Array sh1 in1) = do  
  inputArray0' <- lookupArray in0 -- find the input variables
  inputArray1' <- lookupArray in1  

  ad `seq` newArray ad d  -- create array 
  vs' <- lookupArray ad   -- find the output variables
  

  -- Compute zipWith f -----
  -- outputs sould be placed in "vs"   (Improve names)          
  let vs = fromJust vs' -- HACKITY 
  let inputArray0 = fromJust inputArray0' -- HACKITY
  let inputArray1 = fromJust inputArray1' -- HACKITY
  let ot = getAccType' acc
  let it0 = getAccType' inp0
  let it1 = getAccType' inp1

  fun <- genBFun ot -- output type (of elements)
                 it0 -- input type (of elements)  
                 it1 -- input type (of elements) 
                 f 
                 
  -- all three same dimensionality
  out_dense' <- defineDenseTypesNew ot  d
  inp_dense0' <- defineDenseTypesNew it0 d
  inp_dense1' <- defineDenseTypesNew it1  d

  let inp_vars = varsToList inputArray0 ++ varsToList inputArray1 
  let out_vars = varsToList vs 
  let inp_dense = arBBTypeToList inp_dense0' ++ arBBTypeToList inp_dense1'
  let out_dense = arBBTypeToList out_dense' 

  zipper <- liftArBB$ funDef_ "aap" out_dense inp_dense $ \ out inp -> do
    map_ fun out inp

 ----------
  str <- liftArBB$ serializeFunction_ zipper 
  -- liftIO$ putStrLn "mapee function" 
  liftIO$ putStrLn (getCString str)
 ---------    

  liftArBB$ execute_ zipper out_vars inp_vars  



  ----------------------
          
  return$ Array (sh0) ad
  where
    n = size sh0
    d = dim sh0
    (ad,_) = AD.runArrayData $ (,undefined) `fmap` AD.newArrayData (1024 `max` n)
     


{-
              
------------------------------------------------------------------------------
--
--executeArBB' :: (Typeable aenv, Arrays a) => 
--                OpenAcc aenv a -> 
--                Val aenv -> 
--                GlobalBindings Variable -> 
--                EmitArbb (Result a)  
--executeArBB' acc@(OpenAcc pacc) aenv gv = 
--  case pacc of
                                                                          


------------------------------------------------------------------------------
--  execMap (CLEAN UP) 
execMap :: (Sugar.Elt t') => ArBBType ScalarType -> ArBBType ScalarType -> 
           OpenFun env aenv (t -> t')  -> 
           Result (Array sh t) -> -- input (should be a single array) 
           EmitArbb (Result (Array sh t'))   -- output 
            
execMap ot it f (ResultArray (InternalArray sh v))  = do 
  fun <- genMap ot -- output type (of elements)
                it -- input type (of elemets)  
                f 
  
  out_dense' <- defineDenseTypesNew ot
  inp_dense' <- defineDenseTypesNew it
  out_vars' <- defineGlobalVarsNew out_dense'
  inp_vars' <- defineGlobalVarsNew inp_dense'
  
  assignToVarsImm inp_vars' v
  
  let inp_vars = varsToList inp_vars'
  let out_vars = varsToList out_vars' 
  let inp_dense = arBBTypeToList inp_dense'
  let out_dense = arBBTypeToList out_dense' 

  maper <- funDef_ "aap" out_dense inp_dense $ \ out inp -> do
    map_ fun out inp

 ----------
  str <- serializeFunction_ maper 
  -- liftIO$ putStrLn "mapee function" 
  liftIO$ putStrLn (getCString str)
 ---------    

  execute_ maper out_vars inp_vars  

  let outs =  listToVars v out_vars -- out_vars 
  return (ResultArray (InternalArray sh outs)) -- result of Map is single array 

------------------------------------------------------------------------------
--

execZipWith :: (Sugar.Elt t3) => 
               ArBBType ScalarType -> -- outType
               ArBBType ScalarType -> -- Elements type in array 1
               ArBBType ScalarType -> -- Elements type in array 2
               OpenFun env aenv (t1 -> t2 -> t3)  -> 
               (Result (Array sh t1), Result (Array sh t2)) -> -- input (should be a pair of arrays) 
               EmitArbb (Result (Array sh t3))   -- output 
execZipWith ot it1 it2  f (ResultArray (InternalArray sh1 v1), 
                           ResultArray (InternalArray sh2 v2))  = do 
  fun <- genBFun ot -- output type (of elements)
                 it1 -- input type (of elements)  
                 it2 -- input type (of elements) 
                 f 
  
  out_dense' <- defineDenseTypesNew ot
  inp_dense1' <- defineDenseTypesNew it1
  inp_dense2' <- defineDenseTypesNew it2    
  out_vars' <- defineGlobalVarsNew out_dense'
  inp_vars1' <- defineGlobalVarsNew inp_dense1'
  inp_vars2' <- defineGlobalVarsNew inp_dense2'    
  
  -- TODO: figure out what is correct to do here 
  assignToVarsImm inp_vars1' v1 
  assignToVarsImm inp_vars2' v2 

  
  let inp_vars = varsToList inp_vars1' ++ varsToList inp_vars2'
  let out_vars = varsToList out_vars' 
  let inp_dense = arBBTypeToList inp_dense1' ++ arBBTypeToList inp_dense2' 
  let out_dense = arBBTypeToList out_dense' 

  zipper <- funDef_ "aap" out_dense inp_dense $ \ out inp -> do
    map_ fun out inp

 ----------
  str <- serializeFunction_ zipper
  liftIO$ putStrLn (getCString str)
 ---------    

  execute_ zipper out_vars inp_vars  

  -- Using v1 here in the output states that the 
  -- output has same "shape" as v1 does 
  let outs =  listToVars v1  out_vars -- out_vars 
  return (ResultArray (InternalArray sh1 outs)) 

------------------------------------------------------------------------------
-- 
execFold = error "Fold not implemented" 
-}
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
    --assignTo outs vars -- Working with lists here ! (keep track of where to do what!) 
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



------------------------------------------------------------------------------ 
-- 

--TODO: Figure out how the refcounts are used. see newArray in Execute.hs 
--      in the CUDA backend.
{- 
newArray :: (Sugar.Shape sh, Sugar.Elt e)
         => sh                          -- shape
         -> ExecState (Array sh e)
newArray  sh = do
  -- ad  `seq` mallocArray ad (Just rc) (1 `max` n)
  return $ Array (Sugar.fromElt sh) ad
  where
    n      = Sugar.size sh
    (ad,_) = AD.runArrayData $ (,undefined) `fmap` AD.newArrayData (1024 `max` n)
-}