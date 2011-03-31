{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}



module Data.Array.Accelerate.ArBB.Gen where 

import Data.Array.Accelerate.AST
import qualified Data.Array.Accelerate.Type as Type
import Data.Array.Accelerate.Array.Sugar  (eltType)
import Data.Array.Accelerate.Array.Data (ArrayEltR(..))
import Data.Array.Accelerate.Tuple 
import Data.Array.Accelerate.Analysis.Type


import Intel.ArbbVM
import Intel.ArbbVM.Convenience
 
import Data.Array.Accelerate.ArBB.Type

import Control.Monad
import Control.Applicative hiding (Const) 


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
-- TODO: Notice the REVERSE in genExp on the environment !( FIX THIS !!) 

genExp :: forall env aenv t. 
          OpenExp env aenv t -> ArBBEnv -> EmitArbb [Variable]
genExp (Const c) _ = genConst (eltType (undefined::t)) c 
genExp app@(PrimApp f arg) env = do
   res <- genPrimApp f arg (head (getExpType app)) env
   return [res] 

-- TODO: MAKE SURE THIS IS CORRECT !!! 
--       Reverse down there may be misplaced! 
genExp (Tuple t) env = genTuple t env
genExp (Var idx) env = 
    let n = idxToInt idx
        num = length$ tupleType (eltType (undefined::t))
    in return [(reverse env) !! (n+i) | i <- [0..num-1]]   
--     let n = idxToInt idx 
 --    in case tupleType (eltType (undefined::t)) of 
  --      [_] -> return [(reverse env) !! n] 
    --    cps -> 

--return [(reverse env) !! idxToInt idx] -- use de Bruijn index into environment

genExp p@(Prj idx e)       env = genPrj p env
genExp IndexNil            env = error "genExp: IndexNil not implemented" 
genExp (IndexCons sh i)    env = error "genExp: IndexCons not implemented"
genExp (IndexTail sh)      env = error "genExp: IndexTail not implemented" 
genExp cond@(Cond b t1 t2) env = 
       genCond b t1 t2 (getExpType cond) env

genExp (PrimConst c)       env = error "genExp: PrimConst not implemented" 
genExp (IndexScalar a d)   env = error "genExp: IndexScalar not implemented" 
genExp (Shape a)           env = error "genExp: Shape not implemented" 
genExp (Size a)            env = error "genExp: Size not implemented" 
               


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
   liftIO$ putStrLn ("number of inputs: " ++show (length inputs))
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
genPrim (PrimIDiv _) out inp = do 
   op_ ArbbOpDiv [out] inp
   return out 
genPrim (PrimFDiv _) out inp = do 
   op_ ArbbOpDiv [out] inp 
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
genPrim (PrimSqrt _) out inp = do 
   op_ ArbbOpSqrt [out] inp
   return out
genPrim (PrimExpFloating _) out inp = error "genPrim: EXP not implemented" 
genPrim (PrimLog _) out inp = error "genPrim: LOG not implemented" 

-- BOOLEAN RESULTS 
genPrim (PrimLt _) out inp = do 
   op_ ArbbOpLess [out] inp 
   return out
genPrim (PrimGt _) out inp = do 
   op_ ArbbOpGreater [out] inp
   return out
genPrim (PrimGtEq _) out inp = error "genPrim GEQ not implemented"
genPrim (PrimLtEq _) out inp = error "genPrim LEQ not implemented" 
genPrim (PrimEq _) out inp = error "genPrim EQ not implemented" 

------------------------------------------------------------------------------
-- Tuple Expression! 

-- TODO: Are the results output in "correct" order? 
genTuple :: Tuple (OpenExp env aenv) t -> ArBBEnv -> EmitArbb [Variable] 
genTuple NilTup _ = return [] 
genTuple (SnocTup tup e) env = (++) <$> genTuple tup env <*> genExp e env
{-do 
   vars <- genExp e env 
   rest <- genTuple tup env 
   return (vars ++ rest)  -- Switched order here 31mars
-}


------------------------------------------------------------------------------
-- Conditional

-- TODO test that genCond Works! 
genCond :: OpenExp env aenv Bool -> 
           OpenExp env aenv t ->
           OpenExp env aenv t ->  
           [ScalarType] -> 
           ArBBEnv -> 
           EmitArbb [Variable] 
genCond b t1 t2 st env = do 
   liftIO$ putStrLn$ "Entering uncharted waters! (Conditional)"
   [condition] <- genExp b env  -- should be length one, otherwise error!
   t <- defineTypes st
   result <- defineLocalVars t
   if_ condition 
    ( do 
       r1 <- genExp t1 env 
       zipWithM_ copy_ result r1 
    )
    ( do 
       r2 <- genExp t2 env
       zipWithM_ copy_ result r2 
    ) 
   return result
   error "not implemented"  

-- TODO: CODE DUPLICATION !!!  PUT IN MODULE 
defineLocalVars :: [Type] -> EmitArbb [Variable]
defineLocalVars [] = return [] 
defineLocalVars (t:ts) = do 
  let name = "name"
  v <- createLocal_ t name -- "name" -- name needs to be unique ? 
  vs <- defineLocalVars ts
  return (v:vs) 

-- TODO: CODE DUPLICATION !!!  PUT IN MODULE 
defineTypes :: [ScalarType] -> EmitArbb [Type]
defineTypes [] = return []
defineTypes (x:xs) = do 
   t <- getScalarType_ x 
   ts <- defineTypes xs 
   return (t:ts)

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
-- genPrj

-- The approach might work!( look at codeGenExp) 

genPrj p@(Prj idx e)  env
  = reverse 
  . take (length $ tupleType (expType p)) 
  . drop (prjToInt idx (expType e))
  . reverse 
  <$> do 
        liftIO$ putStrLn$ "Entering uncharted waters (genPrj)"
        liftIO$ putStrLn$ show (length $ tupleType (expType p)) 
        liftIO$ putStrLn$ show (prjToInt idx (expType e))
        vars <- genExp e env
        return vars

------------------------------------------------------------------------------
-- prjToInt 
-- 98% stolen from CUDA backend! 

prjToInt :: TupleIdx t e -> Type.TupleType a -> Int
prjToInt ZeroTupIdx     _                 = 0
prjToInt (SuccTupIdx i) (b `Type.PairTuple` a) = length (tupleType a) + prjToInt i b
prjToInt _ _ =
  error "prjToInt: inconsistent valuation"
------------------------------------------------------------------------------
-- idxToInt -- This is defined in one of the CUDA backend files 
idxToInt :: Idx env t -> Int
idxToInt ZeroIdx       = 0
idxToInt (SuccIdx idx) = 1 + idxToInt idx

