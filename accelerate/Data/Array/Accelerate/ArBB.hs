{-# LANGUAGE GADTs, FlexibleInstances, PatternGuards, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{- 
   -- What is this --
   PatternGuards ? 
   ScopedTypeVariables ? 
-}

module Data.Array.Accelerate.ArBB where 

import Intel.ArbbVM
import Intel.ArbbVM.Convenience

-- import Data.Array.Accelerate hiding (Tuple)
import Data.Array.Accelerate 
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Type as Type

import qualified Data.Array.Accelerate.Smart as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar

import Data.Array.Accelerate.Analysis.Type

import Foreign.Storable as F

--------------------------------------------------------------------------------
-- INITIAL TESTS (HALFWAY DOWN THE FILE FOR MORE SERIOUS ATTEMPT) 
{- 
testa = compMapFunction . Sugar.convertAcc
compMapFunction :: OpenAcc aenv a -> EmitArbb Function
compMapFunction (Map f xs) = compile1to1 f ArbbI32 ArbbI32

-- Compile one input - one output function 
compile1to1 :: OpenFun env aenv t -> ScalarType -> ScalarType -> EmitArbb Function
compile1to1 (Lam (Body body)) otype itype = 
   do 
     sty1 <- getScalarType_ otype
     sty2 <- getScalarType_ itype
     fun <- funDef_ "f" [sty1] [sty2] $ \ [out] [inp] -> do 
       res <- compileBody body [inp]  
       op_ ArbbOpCopy [out] [res]
     return fun
       
compileBody :: OpenExp env aenv t -> [Variable] -> EmitArbb Variable
compileBody (PrimApp f e) inputs = do 
     liftIO$ putStrLn "Apply operation" 
     liftIO$ putStrLn (show e)
     compilePrimApp f e inputs


compilePrimApp ::  PrimFun c -> OpenExp env aenv t -> [Variable] ->  EmitArbb Variable
compilePrimApp (PrimAdd t) (Tuple (SnocTup (SnocTup t1 (Var ix)) c@(Const _))) inputs = do 
     liftIO$ putStrLn "Addition" 
     liftIO$ putStrLn (show t)
     liftIO$ putStrLn (show (idxToInt ix))
     -- liftIO$ putStrLn (show (Sugar.toElem c))
     let index = idxToInt ix
     sty <- getScalarType_ ArbbI32 
     v1 <- compileExp c 
     res <- createLocal_ sty "res"
     --l1  <- int32_ 
     op_ ArbbOpAdd [res] [inputs !! index,v1]
     return res

compileInputs :: Tuple (OpenExp env aenv) t -> [Variable] -> EmitArbb ()
compileInputs NilTup _ = return ()
compileInputs (SnocTup tup e) vars = do 
     compileExp e
     compileInputs tup vars

-- HACK HACK 
--compileExp :: forall t env aenv. OpenExp env aenv t -> EmitArbb  Variable
--compileExp (Const repr) = do 
     --liftIO$ putStrLn $ "YAY!" ++ show (Sugar.toElem repr :: t)
--     int32_ $ read (show (Sugar.toElem repr :: t))
     --return ()

compileConstant :: Elem t => OpenExp env aenv t -> EmitArbb () -- Variable
compileConstant (Const repr) = do
     --liftIO$ putStrLn (show (Sugar.toElem repr :: t)) 
     return ()
-}

--------------------------------------------------------------------------------
-- idxToInt -- This is defined in one of the CUDA backend files 
idxToInt :: Idx env t -> Int
idxToInt ZeroIdx       = 0
idxToInt (SuccIdx idx) = 1 + idxToInt idx

-- TESTING TESTING 
--csTest' = csTest . Sugar.convertAcc
--csTest :: OpenAcc aenv a -> String
--csTest op@(Map f xs)    = "array elems: " ++ show arrayElems ++ "\n" ++ 
--                          "return type: " ++ show returnType
-- where 
--    arrayElems  = getAccType xs
--    returnType  = getAccType op
   
--csTest op@(Fold f i xs) = "id elem: " ++ show itype ++ "\n" ++  
--                          "array elems: "  ++ show arrayElems ++ "\n" ++ 
--                          "return type: " ++ show returnType
-- where 
--    arrayElems  = getAccType xs
--    itype       = getExpType i 
--    returnType  = getAccType op
 

--------------------------------------------------------------------------------
-- More serious attempt 
--------------------------------------------------------------------------------                                                       
-- Attempt at running + compiling into ArBB 
type ArBBEnv = [Variable]

runArBB :: OpenAcc aenv t -> EmitArbb ()   
runArBB op@(Map f a1) = do 
  compileMap (getAccType op) -- output type (of elements) ?
             (getAccType a1) -- input type (of elemets)  
             f 

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

assignToOuts [] [] = return () 
assignToOuts (x:xs) (y:ys) = do 
   op_ ArbbOpCopy [x] [y] 
assignToOuts _ _ = error "Mismatch!"

defineTypes [] = return []
defineTypes (x:xs) = do 
   t <- getScalarType_ x 
   ts <- defineTypes xs 
   return (t:ts)

------------------------------------------------------------------------------
-- generate code for function 
genFun :: OpenFun env aenv t -> ArBBEnv -> EmitArbb [Variable] 
genFun (Lam lam)   = genFun lam 
genFun (Body body) = genExp body 

------------------------------------------------------------------------------
-- genExp 
-- input expression.
-- output Arbb variables holding valuation of expression
genExp :: forall env aenv t. OpenExp env aenv t -> ArBBEnv -> EmitArbb [Variable]
genExp (Const c) _ = genConst (Sugar.eltType (undefined::t)) c 
genExp app@(PrimApp f arg) env = genPrimApp f arg (head (getExpType app)) env
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


genPrimApp :: PrimFun c -> OpenExp env aenv t -> ScalarType -> ArBBEnv -> EmitArbb [Variable]
genPrimApp op args st env = do 
   inputs <- genExp args env
   sty <- getScalarType_ st -- ArbbI32  -- What type is result here ??? (How do I get that type?)
   res <- createLocal_ sty "res" -- needs a unique name? 
   genPrim op res inputs
   return [res]    

genPrim :: PrimFun c -> Variable -> [Variable] -> EmitArbb Variable
genPrim (PrimAdd _) out inputs  = do 
   op_ ArbbOpAdd [out] inputs
   return out 


genTuple :: Tuple (OpenExp env aenv) t -> ArBBEnv -> EmitArbb [Variable] 
genTuple NilTup _ = return [] 
genTuple (SnocTup tup e) env = do 
   vars <- genExp e env 
   rest <- genTuple tup env 
   return (rest ++ vars)
    

--compileInputs :: Tuple (OpenExp env aenv) t -> [Variable] -> EmitArbb ()
--compileInputs NilTup _ = return ()
--compileInputs (SnocTup tup e) vars = do 
--     compileExp e
--     compileInputs tup vars


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





--------------------------------------------------------------------------------
-- Old stuff 
{-
compileScalarFun :: OpenFun env aenv t -> Int
compileScalarFun fun = inputs fun
   where
     inputs :: OpenFun env aenv t -> Int
     inputs (Body _) = 0
     inputs (Lam f)  = 1 + inputs f

-}


{- 
test' = test . Sugar.convertAcc
test :: OpenAcc aenv a -> IO ()
test (Map f xs) = putStrLn $ show xs ++ " " ++ show f
--test (AST.Map f aenv)  = putStrLn "Map Case"

eOpenFun :: OpenFun env aenv t -> IO ()
eOpenFun (Body e@(PrimApp f (Tuple (SnocTup (SnocTup t1 e2) e1))))  = 
  do
   putStrLn $ "BODY" ++ show e
   putStrLn $ show  e1
   putStrLn $ show  e2
eOpenFun (Lam f)   = 
  do   
   putStrLn $ "LAM" 
   eOpenFun f 

eFun :: Fun aenv t -> IO ()
eFun f = eOpenFun f 

eTest' = eTest . Sugar.convertAcc
eTest :: OpenAcc aenv a -> IO ()
eTest (Map f xs) = eFun f  

csfTest' = csfTest . Sugar.convertAcc
csfTest :: OpenAcc aenv a -> Int
csfTest (Map f xs) = compileScalarFun f  
csfTest (Fold f i xs) = compileScalarFun f 
 where 
    types :: [ScalarType]
    types  = codeGenExpType i

csTest' = csTest . Sugar.convertAcc
csTest :: OpenAcc aenv a -> String
csTest (Map f xs)    = "hej"   
csTest (Fold f i xs) = show types 
 where 
    types :: [ScalarType]
    types  = codeGenExpType i

--tTest' = tTest . Sugar.convertAcc
--tTest :: OpenAcc aenv a -> IO ()
--tTest c@(Map f xs) = putStrLn $  show $ accType c



-} 