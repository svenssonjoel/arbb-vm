{-# LANGUAGE GADTs, RankNTypes #-} 
{-# LANGUAGE FlexibleInstances, PatternGuards, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}


------------------------------------------------------------------------------
{- Thougths on Accelerate -> ArBB Bindings
 
   CUDA Bindings generates the program in "stages" that
   correspond to the "kernel"-Skeletons. 
   It may be the case that the ArBB backend can adopt a more 
   unstaged approach. The entire Accelerate program could 
   perhaps be compiled into a single ArBB function. 
   
    + Intermediate arrays managed entirely by ArBB. 
    
    - Only possible if all the Accelerate concepts are 
      implementable entirely within ArBB (That we wont
      need to emulate any accelerate functionality) 
      (This may be possible anyway... ) 

-- About Accelerate

   
   CUDA.hs contains a function called run :: Arrays a => Acc a -> a 
      ArBB backend needs to export the same. 
   AST.hs contains the typeclass Arrays.      

-- Things that I stumble upon using Accelerate
     - Many "different" things have same name. importing module X or Y 
       makes difference!  
       Big difference between using Shape and Sugar.Shape! 
       (This problem occurs again and again).
       You can spend lots of time hunting obscure error messages.

-- About ARBB 
   tip: 

   Use bind_to_host to map an array created using arbb_op_alloc 
   into host memory and then copy data into the "host" side pointer
   This method should have performance benefits if "kernels" are 
   reused. However, I am unsure the method will affect our performance 
   any. It may, though, be a method that works right now, since 
   the copy-in scenario seems buggy.

   Using bind_to_host should still be compatible with the approach 
   I have started but requires "more work" because our code needs
   to perform the actuall copying (instead of the ArBB system taking 
   care of it automatically) 
   

   Thing to consider: 
     The is_remote argument to functions. what does it mean ? 
   

     Needs some kind of "shaped" type description of arrays as well 
       to go along with the "InternalArray" datatype   




-}


module Data.Array.Accelerate.ArBBimm where 

import Intel.ArbbVM
import Intel.ArbbVM.Convenience

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Type as Type
import Data.Array.Accelerate.Array.Sugar  (Array(..), Segments, eltType)
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import qualified Data.Array.Accelerate.Smart       as Sugar
import qualified Data.Array.Accelerate.Array.Data as AD 
import           Data.Array.Accelerate.Array.Data (ArrayEltR(..))

import Data.Array.Accelerate.Array.Representation

import Data.Array.Accelerate.Analysis.Type

import Foreign.Ptr 
import Foreign.Marshal.Array


import Data.Array.Accelerate.ArBB.Data
import Data.Array.Accelerate.ArBB.Type
import Data.Array.Accelerate.ArBB.Gen

import Data.Typeable
import Data.Int

import qualified Data.Map as M

import System.IO.Unsafe

import qualified  Control.Monad.State.Strict as S 
import Control.Monad
#define L S.lift$


    
------------------------------------------------------------------------------
-- run (The entry point)
run :: Arrays a => Sugar.Acc a -> a 
run acc = unsafePerformIO$ arbbSession$ do 
    executeArBB (Sugar.convertAcc acc)

------------------------------------------------------------------------------
-- ExecuteArBB 
executeArBB :: (Typeable aenv, Arrays a) =>  OpenAcc aenv a -> EmitArbb a -- ()  
executeArBB acc = do
 
    let gb = collectGlobals acc (M.empty) 
    glob_vars <- bindGlobals gb 

    resultToArrays =<< executeArBB' acc glob_vars
                  
------------------------------------------------------------------------------
--
executeArBB' :: (Typeable aenv, Arrays a) => 
                OpenAcc aenv a -> 
                GlobalBindings Variable -> 
                EmitArbb (Result a)  
executeArBB' acc@(OpenAcc pacc) gv = 
  case pacc of
     (Use (Array sh ad)) -> do
          let vars = lookupArrayR ad gv
         --  liftIO$ putStrLn$ show vars
          return$  ResultArray (InternalArray sh vars)
     -- TODO: These need consider the "shape" of the input arrays also.
     --       Are they one, two or N dimensional.
     m@(Map f acc) -> execMap (getAccType' (OpenAcc m))  -- output type (of elements)
                              (getAccType'  acc)         -- input type (of elemets)  
                              f =<< executeArBB' acc gv 
     zw@(ZipWith f ac1 ac2) -> execZipWith (getAccType' (OpenAcc zw))
                                           (getAccType' ac1)
                                           (getAccType' ac2) 
                                           f =<< liftM2 (,) (executeArBB' ac1 gv) 
                                                            (executeArBB' ac2 gv)


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
-- This is function definition.. genMap is bad name !! 

-- TODO: Generalise function generation.

genMap :: ArBBType ScalarType -> 
          ArBBType ScalarType -> 
          OpenFun env aenv t -> 
          EmitArbb Function 
genMap out inp fun = do
  out' <- defineTypesNew out
  inp' <- defineTypesNew inp

  let l_inp = arBBTypeToList inp' 
  let l_out = arBBTypeToList out'
  -- Start by generating the function to be mapped!
  fun <- funDef_ "f" l_out l_inp $ \ outs inps -> do 
    vars <- genFun fun inps -- inputs as the "environment"  
    zipWithM_ copy_ outs vars
    --assignTo outs vars -- Working with lists here ! (keep track of where to do what!) 
----------
  str <- serializeFunction_ fun 
  -- liftIO$ putStrLn "mapee function" 
  liftIO$ putStrLn (getCString str)
---------  
    
  return fun


genBFun :: ArBBType ScalarType -> 
           ArBBType ScalarType ->
           ArBBType ScalarType ->  
           OpenFun env aenv t -> 
           EmitArbb Function 
genBFun out inp1 inp2 fun = do
  out' <- defineTypesNew out
  inp1' <- defineTypesNew inp1
  inp2' <- defineTypesNew inp2

  let l_inp = arBBTypeToList inp1' ++ arBBTypeToList inp2'
  let l_out = arBBTypeToList out'
  -- Start by generating the function to be mapped!
  fun <- funDef_ "f" l_out l_inp $ \ outs inps -> do 
    vars <- genFun fun inps -- inputs as the "environment"  
    zipWithM_ copy_ outs vars
    --assignTo outs vars -- Working with lists here ! (keep track of where to do what!) 
----------
  str <- serializeFunction_ fun 
  -- liftIO$ putStrLn "mapee function" 
  liftIO$ putStrLn (getCString str)
---------  
    
  return fun

 
------------------------------------------------------------------------------
-- Assign outputs of something to a list of variables 
--assignTo [] [] = return () 
--assignTo (x:xs) (y:ys) = do 
--   op_ ArbbOpCopy [x] [y] 
--assignTo _ _ = error "AssignTo: Mismatch!"

assignToImm [] [] = return () 
assignToImm (x:xs) (y:ys) = do 
   opImm_ ArbbOpCopy [x] [y] 
assignToImm _ _ = error "AssignTo: Mismatch!"

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

defineDenseTypes :: [ScalarType] -> EmitArbb [Type] 
defineDenseTypes [] = return []
defineDenseTypes (x:xs) = do 
   t <- getScalarType_ x
   d <- getDenseType_ t 1 
   ds <- defineDenseTypes xs 
   return (d:ds)
 

defineLocalVars :: [Type] -> EmitArbb [Variable]
defineLocalVars [] = return [] 
defineLocalVars (t:ts) = do 
  let name = "name"
  -- liftIO$ putStrLn ("Creating local variable: " ++name)    
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

defineTypesNew :: ArBBType ScalarType -> EmitArbb (ArBBType Type)
defineTypesNew ArBBTypeUnit = return ArBBTypeUnit
defineTypesNew (ArBBTypeSingle st)  = do 
   t <- getScalarType_ st 
   return$ ArBBTypeSingle t
defineTypesNew (ArBBTypePair st1 st2) = do 
   t1 <- defineTypesNew st1
   t2 <- defineTypesNew st2 
   return$ ArBBTypePair t1 t2

defineDenseTypesNew :: ArBBType ScalarType -> EmitArbb (ArBBType Type) 
defineDenseTypesNew ArBBTypeUnit = return ArBBTypeUnit
defineDenseTypesNew (ArBBTypeSingle st) = do 
  t <- getScalarType_ st
  d <- getDenseType_ t 1 
  return (ArBBTypeSingle d) 
defineDenseTypesNew (ArBBTypePair st1 st2) = do 
  t1 <- defineDenseTypesNew st1 
  t2 <- defineDenseTypesNew st2 
  return (ArBBTypePair t1 t2) 

 

defineLocalVarsNew :: ArBBType Type -> EmitArbb Vars
defineLocalVarsNew ArBBTypeUnit = return VarsUnit
defineLocalVarsNew (ArBBTypeSingle t) = do 
  let name = "name"
  -- liftIO$ putStrLn ("Creating local variable: " ++name)    
  v <- createLocal_ t name -- "name" -- name needs to be unique ? 
  return$ VarsPrim v
defineLocalVarsNew (ArBBTypePair t1 t2) = do 
  v1 <- defineLocalVarsNew t1
  v2 <- defineLocalVarsNew t2 
  return$ VarsPair v1 v2

defineGlobalVarsNew :: ArBBType Type -> EmitArbb Vars
defineGlobalVarsNew ArBBTypeUnit = return VarsUnit
defineGlobalVarsNew (ArBBTypeSingle t) = do 
  let name = "name"
  b <- getBindingNull_
  -- liftIO$ putStrLn ("Creating local variable: " ++name)    
  gv <- createGlobal_ t name b-- "name" -- name needs to be unique ? 
  v  <- variableFromGlobal_ gv
  return$ VarsPrim v
defineGlobalVarsNew (ArBBTypePair t1 t2) = do 
  v1 <- defineGlobalVarsNew t1
  v2 <- defineGlobalVarsNew t2 
  return$ VarsPair v1 v2


