{-# LANGUAGE GADTs #-}
module Data.Array.Accelerate.ArBB.Type where 


import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Type as Type
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Array.Sugar

import qualified Foreign.Storable as F

import Intel.ArbbVM
import Intel.ArbbVM.Convenience


data ArBBType s 
   = ArBBTypeUnit 
   | ArBBTypeSingle s
   | ArBBTypePair   (ArBBType s) (ArBBType s)

-- same preorder traversal as with the variables in D.A.A.ArBB.Data 
arBBTypeToList :: ArBBType s -> [s] 
arBBTypeToList ArBBTypeUnit = []
arBBTypeToList (ArBBTypeSingle s) = [s] 
arBBTypeToList (ArBBTypePair t1 t2) = 
  let s1 = arBBTypeToList t1 
      s2 = arBBTypeToList t2 
  in s1 ++ s2

getAccType' :: OpenAcc aenv (Array dim e) -> ArBBType ScalarType
getAccType' = tupleType' . accType 

getExpType' :: OpenExp aenv env t -> ArBBType ScalarType
getExpType' = tupleType' . expType 

tupleType' :: Type.TupleType a -> ArBBType ScalarType
tupleType' Type.UnitTuple = ArBBTypeUnit
tupleType' (Type.SingleTuple st)  = ArBBTypeSingle (scalarType st)
-- Swap these on the ArBB side ? (what matches up with preorder?) 
tupleType' (Type.PairTuple t1 t0) = ArBBTypePair (tupleType' t1) 
                                             (tupleType' t0) 
 
getAccType :: OpenAcc aenv (Array dim e) -> [Intel.ArbbVM.ScalarType]
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



