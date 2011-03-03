{-# LANGUAGE GADTs, FlexibleInstances, PatternGuards, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Array.Accelerate.ArBB where 

import Intel.ArbbVM
import Intel.ArbbVM.Convenience


import Data.Array.Accelerate 
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Type as Type

import qualified Data.Array.Accelerate.Smart as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar

import Data.Array.Accelerate.Analysis.Type

import Foreign.Storable as F

------------------------------------------------------------------------------
-- idxToInt -- This is defined in one of the CUDA backend files 
idxToInt :: Idx env t -> Int
idxToInt ZeroIdx       = 0
idxToInt (SuccIdx idx) = 1 + idxToInt idx

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Compile into ArBB calls !
------------------------------------------------------------------------------
-- Attempt at running + compiling into ArBB 
type ArBBEnv = [Variable]

runArBB :: OpenAcc aenv t -> EmitArbb ()   
runArBB op@(Map f a1) = do 
  compileMap (getAccType op) -- output type (of elements) ?
             (getAccType a1) -- input type (of elemets)  
             f 

------------------------------------------------------------------------------
-- What to do in case of Map ? 
compileMap :: [ScalarType] -> [ScalarType] -> OpenFun env aenv t -> EmitArbb () 
compileMap out inp fun = do
  out' <- defineTypes out
  inp' <- defineTypes inp
  -- Start by generating the function to be mapped!
  fun <- funDef_ "f" out' inp' $ \ outs inps -> do 
    vars <- genFun fun inps -- inputs as the "environment"  
    assignToOuts outs vars
  str <- serializeFunction_ fun 
  liftIO$ putStrLn (getCString str)
  return ()

------------------------------------------------------------------------------
-- Assign outputs of something to a list of variables 
assignToOuts [] [] = return () 
assignToOuts (x:xs) (y:ys) = do 
   op_ ArbbOpCopy [x] [y] 
assignToOuts _ _ = error "Mismatch!"

------------------------------------------------------------------------------
-- define ArBB VM types for a list of type "names" 
defineTypes :: [ScalarType] -> EmitArbb [Type]
defineTypes [] = return []
defineTypes (x:xs) = do 
   t <- getScalarType_ x 
   ts <- defineTypes xs 
   return (t:ts)

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
genExp :: forall env aenv t. 
          OpenExp env aenv t -> ArBBEnv -> EmitArbb [Variable]
genExp (Const c) _ = genConst (Sugar.eltType (undefined::t)) c 
genExp app@(PrimApp f arg) env = do 
   res <- genPrimApp f arg (head (getExpType app)) env
   return [res] 
genExp (Tuple t) env = genTuple t env
genExp (Var idx) env = return [env !! idxToInt idx] 
genExp s env = do liftIO$ putStrLn (show s); return [] 

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
   sty <- getScalarType_ st -- ArbbI32  -- What type is result here ??? (How do I get that type?)
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
{- I DONT GET THIS AT ALL!!!! HELP!!!! -}
arbbConst :: Type.ScalarType a -> a -> EmitArbb Variable
arbbConst t@(Type.NumScalarType (Type.IntegralNumType ty)) val
 | Type.IntegralDict <- Type.integralDict ty  -- What is this syntax ??  
  = int32_ val
arbbConst t@(Type.NumScalarType (Type.FloatingNumType (Type.TypeFloat _))) val
  = float32_ val
arbbConst t@(Type.NumScalarType (Type.FloatingNumType (Type.TypeDouble _))) val
  = float64_ val
-- TODO: Keep going for all Accelerate Types

--------------------------------------------------------------------------------
-- Type Machinery!!!! 
 
getAccType :: OpenAcc aenv (Sugar.Array dim e) -> [Intel.ArbbVM.ScalarType]
getAccType =  tupleType . accType

getExpType :: OpenExp aenv env t -> [Intel.ArbbVM.ScalarType]
getExpType =  tupleType . expType

tupleType :: Type.TupleType a -> [Intel.ArbbVM.ScalarType]
tupleType Type.UnitTuple         = []
tupleType (Type.SingleTuple  ty) = [scalarType ty]
tupleType (Type.PairTuple t1 t0) = tupleType t1 ++ tupleType t0

scalarType :: Type.ScalarType a -> Intel.ArbbVM.ScalarType
scalarType (Type.NumScalarType    ty) = numType ty
scalarType (Type.NonNumScalarType ty) = nonNumType ty

numType :: Type.NumType a -> Intel.ArbbVM.ScalarType
numType (Type.IntegralNumType ty) = integralType ty
numType (Type.FloatingNumType ty) = floatingType ty

integralType :: Type.IntegralType a -> Intel.ArbbVM.ScalarType
integralType (Type.TypeInt8    _) = ArbbI8
integralType (Type.TypeInt16   _) = ArbbI16
integralType (Type.TypeInt32   _) = ArbbI32
integralType (Type.TypeInt64   _) = ArbbI64
integralType (Type.TypeWord8   _) = ArbbU8
integralType (Type.TypeWord16  _) = ArbbU16
integralType (Type.TypeWord32  _) = ArbbU32
integralType (Type.TypeWord64  _) = ArbbU64
integralType (Type.TypeCShort  _) = ArbbI16  -- correct ?
integralType (Type.TypeCUShort _) = ArbbU16  -- correct ? 
integralType (Type.TypeCInt    _) = ArbbI32  -- Fix these
integralType (Type.TypeCUInt   _) = ArbbU32  
integralType (Type.TypeCLong   _) = ArbbI32 
integralType (Type.TypeCULong  _) = ArbbU32
integralType (Type.TypeCLLong  _) = ArbbI64
integralType (Type.TypeCULLong _) = ArbbU64

integralType (Type.TypeInt     _) =
  case F.sizeOf (undefined::Int) of
       4 -> ArbbI32
       8 -> ArbbI64
       _ -> error "we can never  here"

integralType (Type.TypeWord    _) =
  case F.sizeOf (undefined::Int) of
       4 -> ArbbU32
       8 -> ArbbU64
       _ -> error "we can never  here"

floatingType :: Type.FloatingType a -> Intel.ArbbVM.ScalarType
floatingType (Type.TypeFloat   _) = ArbbF32
floatingType (Type.TypeDouble  _) = ArbbF64
floatingType (Type.TypeCFloat  _) = ArbbF32
floatingType (Type.TypeCDouble _) = ArbbF64

nonNumType :: Type.NonNumType a -> Intel.ArbbVM.ScalarType
nonNumType (Type.TypeBool   _) = ArbbBoolean
nonNumType (Type.TypeChar   _) = ArbbI8
nonNumType (Type.TypeCChar  _) = ArbbI8
nonNumType (Type.TypeCSChar _) = ArbbI8 -- huh ? (Signed char)  
nonNumType (Type.TypeCUChar _) = ArbbU8




