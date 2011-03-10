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
      to emulate any accelerate functionality) 

-- About Accelerate

   
   CUDA.hs contains a function called run :: Arrays a => Acc a -> a 
      ArBB backend needs to export the same. 
   AST.hs contains the typeclass Arrays.      



-}



module Data.Array.Accelerate.ArBB where 

import Intel.ArbbVM
import Intel.ArbbVM.Convenience

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Type as Type
import Data.Array.Accelerate.Array.Sugar  (Array(..), Segments, eltType)
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import qualified Data.Array.Accelerate.Array.Data as AD 

import Data.Array.Accelerate.Array.Representation

import Data.Array.Accelerate.Analysis.Type


--import Foreign.Storable as F
import Foreign.Ptr 
import Foreign.Marshal.Array



import Data.Array.Accelerate.ArBB.Data
import Data.Array.Accelerate.ArBB.Type

import Data.Typeable
import Data.Int

import qualified Data.Map as M


type ArBBEnv = [Variable]
       
------------------------------------------------------------------------------
-- run (The entry point)
run :: Arrays a => Acc a -> a 
run acc = undefined 


executeArBB :: (Typeable aenv, Typeable a) => OpenAcc aenv a -> EmitArbb ()
executeArBB acc = do
    let gb = collectGlobals acc (M.empty)
    glob_vars <- bindGlobals gb
  
    dummy <- getScalarType_ ArbbI32
    dt    <- getDenseType_ dummy 1 
    -- An ArBB function with no inputs. (Ok ? ) 
    -- Todo: no input functions seems to be ok. 
    --       "binding" within a function does not seems to be ok 
    --       but small tests does not produce same error as this! 
    fun <- funDef_ "main" [dt] [] $ \ o [] -> do 
       o1 <- executeArBB' acc glob_vars
       assignTo o o1 
       return () 


----------
    str <- serializeFunction_ fun 
    liftIO$ putStrLn (getCString str)
---------  

      
    
    withArray_ (replicate 200 0 :: [Int32]) $ \ out -> do 
      outb <- createDenseBinding_ (castPtr out) 1 [200] [4]
      gout <- createGlobal_ dt "output" outb  
      vout <- variableFromGlobal_ gout 
      
      execute_ fun [vout] []
    
  
      result <- liftIO$ peekArray 1 out
      liftIO$ putStrLn (show result)
      return ()
    
    return ()
    


executeArBB' :: (Typeable aenv, Typeable a) => 
                OpenAcc aenv a -> 
                GlobalBindings Variable -> 
                EmitArbb [Variable] 
executeArBB' acc@(OpenAcc pacc) gv = 
  case pacc of
     (Use (Array sh ad)) -> return (lookupArray ad gv) 
     m@(Map f acc) -> execMap (getAccType (OpenAcc m))  -- output type (of elements)
                              (getAccType  acc) -- input type (of elemets)  
                              f =<< executeArBB' acc gv 

        
execMap ot it f inputs = do 
  fun <- genMap ot -- output type (of elements)
                it -- input type (of elemets)  
                f 
  
  out_dense <- defineDenseTypes ot
  inp_dense <- defineDenseTypes it
  out_vars  <- defineLocalVars out_dense
  inp_vars  <- defineLocalVars inp_dense

  assignTo inp_vars inputs
  -- maper <- funDef_ "mapf" out_dense inp_dense $ \ outs inps -> do 
  
  map_ fun out_vars inp_vars
     
     
  return out_vars

{-
  -- BIG CHEATY PART STARTS HERE 
  withArray_ (replicate 10 0 :: [Int32]) $ \ out -> do 
    outb <- createDenseBinding_ (castPtr out) 1 [10] [4]
    gout <- createGlobal_ (out_dense !! 0) "output" outb -- Cheat! 
    vout <- variableFromGlobal_ gout 
    execute_ maper [vout] input_vars
    
    result <- liftIO$ peekArray 10 out
    liftIO$ putStrLn (show result)

  
  return [] 
  -}


{-
bArray :: (Array sh e) 
         -> EmitArbb [Variable]
bArray (Array sh ad) = do
   let n = size sh
   bindArray ad n
  -}
------------------------------------------------------------------------------ 
-- Experiment: Execute OpenAcc
{- 
executeArBB :: OpenAcc aenv a -> EmitArbb [Variable] 
executeArBB (Use (Array sh ad)) = let n = size sh 
                                  in  bindArray ad n
executeArBB m@(Map f ac) = do 
  mapfun <- genArBB m -- This one cheats a bit at the moment 
  a <- executeArBB ac -- get the inputs to map 
  let inpT = getAccType ac -- Gives an ArBB Type
      outT = getAccType m 
  out_dense <- defineDenseTypes outT
  inp_dense <- defineDenseTypes inpT

  maper <- funDef_ "mapf" out_dense inp_dense $ \ outs inps -> do 
    map_ mapfun outs inps 
   
  
  -- BIG CHEATY PART STARTS HERE 
  withArray_ (replicate 10 0 :: [Int32]) $ \ out -> do 
    outb <- createDenseBinding_ (castPtr out) 1 [10] [4]
    gout <- createGlobal_ (out_dense !! 0) "output" outb -- Cheat! 
    vout <- variableFromGlobal_ gout 
    execute_ maper [vout] a
    
    result <- liftIO$ peekArray 10 out
    liftIO$ putStrLn (show result)

  return [undefined] 
-} 

------------------------------------------------------------------------------
-- generate ArBB functions from AST Nodes
-- TODO: Fix map case... 
{-
genArBB :: OpenAcc aenv t -> EmitArbb Function 
genArBB op@(Map f a1) = do  -- Emit a function that takes an array 
  fun <- genMap (getAccType op) -- output type (of elements)
                (getAccType a1) -- input type (of elemets)  
                f 
  return fun
-}  
------------------------------------------------------------------------------
-- What to do in case of Map ? 
 
genMap :: [ScalarType] -> [ScalarType] -> OpenFun env aenv t -> EmitArbb Function 
genMap out inp fun = do
  out' <- defineTypes out
  inp' <- defineTypes inp
  -- Start by generating the function to be mapped!
  fun <- funDef_ "f" out' inp' $ \ outs inps -> do 
    vars <- genFun fun inps -- inputs as the "environment"  
    assignTo outs vars
----------
  str <- serializeFunction_ fun 
  liftIO$ putStrLn "mapee function" 
  liftIO$ putStrLn (getCString str)
---------  
    
  return fun
 
------------------------------------------------------------------------------
-- Assign outputs of something to a list of variables 
assignTo [] [] = return () 
assignTo (x:xs) (y:ys) = do 
   op_ ArbbOpCopy [x] [y] 
assignTo _ _ = error "AssignTo: Mismatch!"

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
  v <- createLocal_ t "name" -- name needs to be unique ? 
  vs <- defineLocalVars ts
  return (v:vs) 

------------------------------------------------------------------------------
-- generate code for function 
-- Bunch of Lambdas followed by a Body 
-- The body uses "De Bruijn" indices (so no variable name binding in lam)  
genFun :: OpenFun env aenv t -> ArBBEnv -> EmitArbb [Variable] 
genFun (Lam lam)   = genFun lam 
genFun (Body body) = genExp body 

------------------------------------------------------------------------------
-- genExp 
-- input expression.
-- output Arbb variables holding valuation of expression

-- TODO: Some Expressions contain arrays (IndexScalar) 
--       The Accelerate guys uses the "liftAcc" machinery
--       in the Execute module to address this issue. 

genExp :: forall env aenv t. 
          OpenExp env aenv t -> ArBBEnv -> EmitArbb [Variable]
genExp (Const c) _ = genConst (eltType (undefined::t)) c 
genExp app@(PrimApp f arg) env = do 
   res <- genPrimApp f arg (head (getExpType app)) env
   return [res] 
genExp (Tuple t) env = genTuple t env
genExp (Var idx) env = return [env !! idxToInt idx] 
--genExp s env = do liftIO$ putStrLn (show s); return [] 

genConst :: Type.TupleType a -> a -> EmitArbb [Variable]
genConst Type.UnitTuple  _       = return [] 
genConst (Type.SingleTuple ty) c = do
  s <- arbbConst ty c 
  return [s]
genConst (Type.PairTuple ty1 ty0) (cs,c) = do
  s1 <- genConst ty1 cs
  s2 <- genConst ty0 c 
  return (s1 ++ s2)  

------------------------------------------------------------------------------
-- genPrimApp
-- Is it possible to have a "multi-scalar" result from a primApp? 
--   (Complex number?) 
--   I'm going with that the answer is no. 
-- 
-- Inputs: 
--  Function
--  Inputs
--  Type of output 
--  Environment       
genPrimApp :: PrimFun c -> 
              OpenExp env aenv t -> 
              ScalarType -> 
              ArBBEnv -> 
              EmitArbb Variable
genPrimApp op args st env = do 
   inputs <- genExp args env
   sty <- getScalarType_ st  -- What type is result here ??? (How do I get that type?)
   res <- createLocal_ sty "res" -- needs a unique name? 
   genPrim op res inputs
   return res    

------------------------------------------------------------------------------
-- Primitive operations (ADD, SUB, MUL etc) 
genPrim :: PrimFun c -> Variable -> [Variable] -> EmitArbb Variable
genPrim (PrimAdd _) out inputs = do 
   op_ ArbbOpAdd [out] inputs
   return out 
genPrim (PrimSub _) out inputs = do 
   op_ ArbbOpSub [out] inputs
   return out 
genPrim (PrimMul _) out inputs = do 
   op_ ArbbOpMul [out] inputs 
   return out


------------------------------------------------------------------------------
-- Tuple Expression! 
genTuple :: Tuple (OpenExp env aenv) t -> ArBBEnv -> EmitArbb [Variable] 
genTuple NilTup _ = return [] 
genTuple (SnocTup tup e) env = do 
   vars <- genExp e env 
   rest <- genTuple tup env 
   return (rest ++ vars)

------------------------------------------------------------------------------
-- More type machinery ! 
arbbConst :: Type.ScalarType a -> a -> EmitArbb Variable
arbbConst t@(Type.NumScalarType (Type.IntegralNumType ty)) val
 | Type.IntegralDict <- Type.integralDict ty  -- What is this syntax ??  
  = int32_ val
arbbConst t@(Type.NumScalarType (Type.FloatingNumType (Type.TypeFloat _))) val
  = float32_ val
arbbConst t@(Type.NumScalarType (Type.FloatingNumType (Type.TypeDouble _))) val
  = float64_ val
-- TODO: Keep going for all Accelerate Types

------------------------------------------------------------------------------
-- idxToInt -- This is defined in one of the CUDA backend files 
idxToInt :: Idx env t -> Int
idxToInt ZeroIdx       = 0
idxToInt (SuccIdx idx) = 1 + idxToInt idx




