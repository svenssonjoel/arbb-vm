{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE ScopedTypeVariables #-}





module Data.Array.Accelerate.ArBB.Gen where 

import Data.Array.Accelerate.AST
import qualified Data.Array.Accelerate.Type as Type
import Data.Array.Accelerate.Array.Sugar  (eltType)
import Data.Array.Accelerate.Array.Data (ArrayEltR(..))
import Data.Array.Accelerate.Tuple 

import Intel.ArbbVM
import Intel.ArbbVM.Convenience
 
import Data.Array.Accelerate.ArBB.Type


type ArBBEnv = [Variable]    
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
                       -- use  Debruijn to index into env 


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
   sty <- getScalarType_ st 
   let resname = "res" -- needs a unique name? 
   -- liftIO$ putStrLn ("Creating a result variable: " ++resname)
   res <- createLocal_ sty resname
   genPrim op res inputs
   return res    

------------------------------------------------------------------------------
-- Primitive operations (ADD, SUB, MUL etc) 
genPrim :: PrimFun c -> Variable -> [Variable] -> EmitArbb Variable
genPrim (PrimAdd _) out inp = do 
   op_ ArbbOpAdd [out] inp
   return out 
genPrim (PrimSub _) out inp = do 
   op_ ArbbOpSub [out] inp
   return out 
genPrim (PrimMul _) out inp = do 
   op_ ArbbOpMul [out] inp 
   return out
genPrim (PrimQuot _) out inp = error "genPrim: QUOTIENT not implemented" 
genPrim (PrimRem _)  out inp = error "genPrim: REMINDER not implemented" 
genPrim (PrimMod _)  out inp = do 
   op_ ArbbOpMod [out] inp
   return out 

-- Unary things.. (Will they work) 
genPrim (PrimNeg _) out inp = do 
   op_ ArbbOpNeg [out] inp
   return out 
genPrim (PrimAbs _) out inp = do 
   op_ ArbbOpAbs [out] inp 
   return out
genPrim (PrimSig _) out inp = error "genPrim: SIGNUM not implemented" 


------------------------------------------------------------------------------
-- Tuple Expression! 
genTuple :: Tuple (OpenExp env aenv) t -> ArBBEnv -> EmitArbb [Variable] 
genTuple NilTup _ = return [] 
genTuple (SnocTup tup e) env = do 
   vars <- genExp e env 
   rest <- genTuple tup env 
   return (rest ++ vars)

------------------------------------------------------------------------------
-- Constants of Various Type
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

