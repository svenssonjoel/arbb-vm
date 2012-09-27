{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# OPTIONS  -XDeriveDataTypeable #-}

module Intel.ArbbVM ( Context, ErrorDetails, Type, Variable, 
                      GlobalVariable, Binding, Function(..), VMString, 
                      AttributeMap, 
                      
                      Error(..), ScalarType(..), Opcode(..), 
                      CallOpcode(..), 
                      LoopType(..), LoopBlock(..), RangeAccessMode(..), 
                      AttributeType(..),
                      
                      ArbbVMException, 
                      
                      module Intel.ArbbVM.Debug,

                      getDefaultContext, getScalarType, 
                      
                      getErrorMessage, getErrorCode, freeErrorDetails, 
                      
                      sizeOf,
                      
                      functionToRefCountable,globalVariableToRefCountable,
                      acquireRef, releaseRef, 
                      

                      getDenseType, createGlobal, createGlobalNB, 
                      getNestedType, 
		      
		      -- Removed in latest ArBB
                      --isBindingNull, getBindingNull, 
                      nullBinding,
                      
                      createDenseBinding, freeBinding, getFunctionType,
                      beginFunction, endFunction, 
                      op, opImm, opDynamic, opDynamicImm,
		      callOp, execute, 
                      
                      -- Compile has changed into compile_for_args in latest version
                      --compile, 
		  
                      finish, createConstant, createLocal,
                      variableFromGlobal, getParameter, readScalar,
                      writeScalar, serializeFunction, freeVMString,
                      getCString, 
                      
                      beginLoop, beginLoopBlock, loopCondition,
                      endLoop, break, continue, ifBranch, elseBranch,
                      endIf,
                      
                      mapToHost, 
                      setHeapSize, setDecompDegree, setNumThreads,

-- REMOVE THIS EVENTUALLY
                      fromContext 
-------------------------
                       ) where

import Intel.ArbbVM.Debug

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable hiding (sizeOf)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import Data.Typeable
import Data.Word
import Data.IORef

import System.IO.Unsafe (unsafePerformIO)

--import C2HS hiding (sizeOf) 

import Prelude hiding (break)

-- find this file in the ArBB installation!
-- #include "../cbits/arbb_vmapi.h"
#include <arbb_vmapi.h>
-- ----------------------------------------------------------------------

newtype Context = Context {fromContext :: Ptr ()} 
newtype ErrorDetails = ErrorDetails {fromErrorDetails :: Ptr ()} 
newtype Type = Type {fromType :: Ptr ()} 
newtype Variable = Variable {fromVariable :: Ptr ()}
newtype GlobalVariable = GlobalVariable {fromGlobalVariable :: Ptr ()}
newtype Binding = Binding {fromBinding :: Ptr ()}
newtype Function = Function {fromFunction :: Ptr ()}
newtype VMString = VMString {fromVMString :: Ptr ()}

newtype RefCountable = RefCountable {fromRefCountable :: Ptr () }
-- types to support aux functionality
newtype AttributeMap = AttributeMap {fromAttributeMap :: Ptr ()}
-- TODO: there is a struct called arbb_attribute_key_value_t 
--       that needs to be handeld.

-------------------------------------------------------------------------
-- 
cIntConv :: (Integral a, Integral b) => a -> b 
cIntConv = fromIntegral 
cToEnum  = toEnum . fromIntegral 

cFromEnum :: (Enum a, Integral i)  => a -> i 
cFromEnum = fromIntegral . fromEnum
-- ----------------------------------------------------------------------
-- ENUMS
-- ----------------------------------------------------------------------
{# enum arbb_error_t as Error 
   {underscoreToCase} deriving (Show, Eq) #}

{# enum arbb_scalar_type_t as ScalarType 
   {underscoreToCase} deriving (Show, Eq) #}

{# enum arbb_opcode_t as Opcode 
   {underscoreToCase} deriving (Show, Eq) #}

{# enum arbb_call_opcode_t as CallOpcode 
   {underscoreToCase} deriving (Show, Eq) #}

{# enum arbb_loop_type_t as LoopType 
   {underscoreToCase} deriving (Show, Eq) #}

{# enum arbb_loop_block_t as LoopBlock
   {underscoreToCase} deriving (Show, Eq) #} 

{# enum arbb_range_access_mode_t as RangeAccessMode 
   {underscoreToCase} deriving (Show, Eq) #}

-- enums to support aux functionality 
{# enum arbb_attribute_type_t as AttributeType 
   {underscoreToCase} deriving (Show, Eq) #}
 
-- ----------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------
-- Todo: This is code duplication, clean up
peekErrorDet  ptr = do { res <- peek ptr; return $ ErrorDetails res}
peekType  ptr = do { res <- peek ptr; return $ Type res}    
peekFunction  ptr = do { res <- peek ptr; return $ Function res}    
peekGlobalVariable ptr = do { res <- peek ptr; return $ GlobalVariable res} 
peekVariable ptr = do { res <- peek ptr; return $ Variable res} 
peekContext  ptr = do { res <- peek ptr; return $ Context res}    
peekBinding  ptr = do { res <- peek ptr; return $ Binding res}         
peekVMString ptr = do { res <- peek ptr; return $ VMString res} 
peekRefCountable ptr = do { res <- peek ptr; return $ RefCountable res}

withTypeArray inp = if (length inp) == 0 then withNullPtr 
	             else withArray (fmap fromType inp) 
                          
withVariableArray = withArray . (fmap fromVariable) 
withIntArray xs = withArray (fmap fromIntegral xs)

withNullPtr :: (Ptr b -> IO a) -> IO a 
withNullPtr f = f nullPtr

alloc3 = allocaArray 3
peek3 d =
  do
    l <- peekArray 3 d :: IO [CULong]
    return (map fromIntegral l)
-- ----------------------------------------------------------------------
-- Exception
-- ----------------------------------------------------------------------

data ArbbVMException = ArbbVMException Error String
  deriving (Eq, Show, Typeable)


instance Exception ArbbVMException 

-- Debug + Error handling is changing.. see ArbbVM/Debug

throwIfErrorIO2 :: (Error,a,b,ErrorDetails) -> IO (a,b) 
throwIfErrorIO2 (error_code,a,b,error_det) = 
   if fromEnum error_code > 0 
    then do 
      str <- getErrorMessage error_det
      freeErrorDetails error_det
      throwIO (ArbbVMException error_code str)
    else return (a,b)

throwIfErrorIO1 :: (Error,a,ErrorDetails) -> IO a 
throwIfErrorIO1 (error_code,a,error_det) = 
   if fromEnum error_code > 0 
    then do 
      str <- getErrorMessage error_det
      freeErrorDetails error_det
      throwIO (ArbbVMException error_code str)
    else return a
    
throwIfErrorIO0 :: (Error,ErrorDetails) -> IO ()
throwIfErrorIO0 (error_code, error_det) = 
   throwIfErrorIO1 (error_code, (), error_det)  





-- ----------------------------------------------------------------------
-- BINDINGS 
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- To and from refCountable 

-- TODO: Include these in debug traces ? 
{# fun unsafe arbb_function_to_refcountable as functionToRefCountable 
   { fromFunction `Function' } ->   `RefCountable' RefCountable #}

{# fun unsafe arbb_global_variable_to_refcountable as globalVariableToRefCountable
   { fromGlobalVariable `GlobalVariable' } -> `RefCountable' RefCountable #} 

-- ----------------------------------------------------------------------
-- acquire and relese references

acquireRef rc = 
  acquireRef' rc >>= 
  dbg0 "arbb_acquire_ref" [("refCountable", show $ fromRefCountable rc)] >>=
  throwIfErrorIO0 
 

{# fun unsafe arbb_acquire_ref as acquireRef'  
   { fromRefCountable `RefCountable' , 
     alloca- `ErrorDetails' peekErrorDet*  } -> `Error' cToEnum #} 


releaseRef rc = 
  releaseRef' rc >>= 
  dbg0 "arbb_release_ref" [("refCountable", show $ fromRefCountable rc)] >>= 
  throwIfErrorIO0 

{# fun unsafe arbb_release_ref as releaseRef' 
   { fromRefCountable `RefCountable' , 
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 






-- ----------------------------------------------------------------------
-- getDefaultContext 
-- Inputs: None
-- Outputs: The default context.

getDefaultContext :: IO Context
getDefaultContext =
    getDefaultContext' >>= 
    newDBGFile >>=  
    dbg "arbb_get_default_context" [] ("ctx",fromContext) >>= 
    throwIfErrorIO1

{# fun unsafe arbb_get_default_context as getDefaultContext' 
   { alloca- `Context' peekContext* , 
     alloca- `ErrorDetails' peekErrorDet*   } -> `Error' cToEnum #} 
    -- id      `Ptr (Ptr ())'  } -> `Error' cToEnum #} 


    

-- ----------------------------------------------------------------------
-- getScalarType. 

getScalarType :: Context -> ScalarType -> IO Type
getScalarType ctx st = 
    getScalarType' ctx st  >>= 
    dbg "arbb_get_scalar_type" [("ctx",show $ fromContext ctx),
                                ("st" ,show st)]  ("type",fromType) >>= 
    throwIfErrorIO1
   
{# fun unsafe arbb_get_scalar_type as getScalarType' 
   { fromContext `Context' , 
     alloca-     `Type' peekType* ,
     cFromEnum   `ScalarType' ,
     alloca-     `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}
    -- id `Ptr (Ptr ())'  
  
     

-- ----------------------------------------------------------------------
-- Error handling 

   
{# fun unsafe arbb_get_error_message as getErrorMessage 
   { fromErrorDetails `ErrorDetails' } -> `String' #} 


{# fun unsafe arbb_get_error_code as getErrorCode 
   { fromErrorDetails `ErrorDetails' } -> `Error' cToEnum #}


{# fun unsafe arbb_free_error_details as freeErrorDetails 
   { fromErrorDetails `ErrorDetails' } -> `()' #}


-- ----------------------------------------------------------------------
-- sizeOf 

sizeOf ctx t = sizeOf' ctx t >>= throwIfErrorIO1
  
{# fun unsafe arbb_sizeof_type as sizeOf' 
   { fromContext `Context'    ,
     alloca-     `Word64' peekCULLong*    ,
     fromType    `Type'       , 
     alloca-     `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}
  where peekCULLong x = 
           do 
            res <- peek x 
            return (fromIntegral res)


-- ----------------------------------------------------------------------
-- getDenseType
getDenseType ctx t dim = 
  getDenseType' ctx t dim >>= 
   dbg "arbb_get_dense_type" [("ctx",show $ fromContext ctx),
                              ("dt" ,show $ fromType t),
                              ("dim",show dim)]  ("type",fromType) >>= 
  throwIfErrorIO1
            
{# fun unsafe arbb_get_dense_type as getDenseType' 
   { fromContext `Context'   ,
     alloca-     `Type'   peekType* ,
     fromType    `Type'      , 
     cIntConv    `Int'       ,
     alloca-     `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}  


getNestedType ctx t = 
    getNestedType' ctx t >>= 
    dbg "arbb_get_nested_type" [("ctx", show $ fromContext ctx),
                               ("itype", show $ fromType t)]
                               ("otype", fromType) 
                               >>=
    throwIfErrorIO1

{#fun unsafe arbb_get_nested_type as getNestedType'
   { fromContext `Context'  , 
     alloca-  `Type' peekType*, 
     fromType `Type'    , 
     alloca-  `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}


-- ----------------------------------------------------------------------
-- createGlobal 

createGlobalNB :: Context -> Type -> String -> IO GlobalVariable
createGlobalNB ctx t name = 
   createGlobal' ctx t name (Binding nullPtr) nullPtr >>= 
   dbg "arbb_create_global" [("ctx",show $ fromContext ctx),
                             ("t" ,show $ fromType t),
                             ("name",name)]  ("GlobalVar",fromGlobalVariable) >>=                               
   throwIfErrorIO1 


createGlobal :: Context -> Type -> String -> Binding -> IO GlobalVariable
createGlobal ctx t name b = 
   createGlobal' ctx t name b nullPtr >>= 
   dbg "arbb_create_global" [("ctx",show $ fromContext ctx),
                             ("t" ,show $ fromType t),
                             ("name",name),
                             ("bind",show $ fromBinding b)]  ("GlobalVar",fromGlobalVariable) >>=                     
   throwIfErrorIO1 
             
{# fun unsafe arbb_create_global as createGlobal'
   { fromContext  `Context'     ,
     alloca-      `GlobalVariable' peekGlobalVariable* , 
     fromType     `Type'        ,
     withCString* `String'      , 
     fromBinding  `Binding'     , 
     id           `Ptr ()'      ,
     alloca-      `ErrorDetails' peekErrorDet*  } -> `Error' cToEnum #} 
     

     --id           `Ptr (Ptr ())'
     
-- ----------------------------------------------------------------------
-- Bindings

-- The is_binding_null API call is removed in the latest ArBB version
--{# fun pure arbb_is_binding_null as isBindingNull
--   { fromBinding `Binding' } -> `Bool'  cToBool #} 

nullBinding = Binding nullPtr
-- #OLD# TODO: see if this needs to be done differently.
-- The set_binding_null API call i removed in latest ArBB version
-- {# fun unsafe arbb_set_binding_null as getBindingNull 
--   { alloca- `Binding' peekBinding*  } -> `()'#} 
  

createDenseBinding ::  Context -> Ptr () -> Word -> [Word64] -> [Word64] ->  IO Binding
createDenseBinding ctx d dim sizes pitches = 
  createDenseBinding' ctx d dim sizes pitches >>= 
   dbg "arbb_create_densebinding" [("ctx",show $ fromContext ctx),
                                   ("dataPtr" ,show $ d),
                                   ("dim", show dim),
                                   ("sizes", show sizes),
                                   ("pitches",show pitches)]                                 
                                   ("bind",fromBinding) >>=                               
  throwIfErrorIO1
           
{# fun unsafe arbb_create_dense_binding as createDenseBinding'  
   { fromContext `Context'  ,
     alloca- `Binding' peekBinding* ,
     id `Ptr ()' ,
     cIntConv `Word' ,
     withIntArray* `[Word64]',
     withIntArray* `[Word64]', 
     alloca-      `ErrorDetails' peekErrorDet*  } -> `Error' cToEnum #}


freeBinding ctx bind = 
  freeBinding' ctx bind >>= throwIfErrorIO0 

{# fun unsafe arbb_free_binding as freeBinding'
   { fromContext `Context'  ,
     fromBinding `Binding'  ,
     alloca-      `ErrorDetails' peekErrorDet*     } -> `Error' cToEnum #}

   

-- ----------------------------------------------------------------------
-- FUNCTIONS 

getFunctionType :: Context -> [Type] -> [Type] -> IO Type
getFunctionType ctx outp inp = 
  do 
    let outlen = length outp
        inlen  = length inp
    getFunctionType' ctx outlen outp inlen inp >>=  
      dbg "arbb_get_function_type" [("ctx",show $ fromContext ctx),
                                    ("outputs" ,show (map fromType outp)),
                                    ("inputs", show (map fromType inp))]
                                    ("type",fromType) >>=                               
      throwIfErrorIO1 
 
{# fun unsafe arbb_get_function_type as getFunctionType' 
   { fromContext `Context'     ,
     alloca- `Type' peekType*  , 
     cIntConv `Int'            , 
     withTypeArray* `[Type]'   , 
     cIntConv `Int'            , 
     withTypeArray* `[Type]'   ,
     alloca-      `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}

beginFunction :: Context -> Type -> String -> Int -> IO Function
beginFunction ctx t name remote = 
  beginFunction' ctx t name remote >>= 
  dbg "arbb_begin_function" [("ctx",show $ fromContext ctx),
                             ("fn_t" ,show $ fromType t),
                             ("name", name),
                             ("remote", show remote)]
                             ("fun",fromFunction) >>=                               
                                    
  throwIfErrorIO1 

{# fun unsafe arbb_begin_function as beginFunction'
   { fromContext `Context'   ,
     alloca- `Function' peekFunction* ,
     fromType `Type'   ,
     withCString* `String'  ,
     cIntConv     `Int'    ,
     alloca-      `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}

endFunction :: Function -> IO ()
endFunction f = 
   endFunction' f  >>= 
   dbg0 "arbb_end_function" [("fun",show $ fromFunction f)] >>=                               
   throwIfErrorIO0
 
{#fun unsafe arbb_end_function as endFunction' 
      { fromFunction `Function'    ,
        alloca- `ErrorDetails' peekErrorDet*  } -> `Error' cToEnum #}


-- ----------------------------------------------------------------------
-- Operations of various kinds

-- Operations on scalaras
opImm :: Opcode -> [Variable] -> [Variable] -> IO ()
opImm opcode outp inp = 
    op' (Function nullPtr) opcode outp inp nullPtr nullPtr >>= 
    dbg0 "arbb_op" [("fun", show $ nullPtr),
     	 	    ("Opcode", show opcode),
		    ("outputs", show (map fromVariable outp)),
		    ("inputs" , show (map fromVariable inp))] >>=
    throwIfErrorIO0  
 
op :: Function -> Opcode -> [Variable] -> [Variable] -> IO ()
op f opcode outp inp = 
    op' f opcode outp inp nullPtr nullPtr >>= 
    dbg0 "arbb_op" [("fun",show $ fromFunction f),
                    ("Opcode", show opcode),
                    ("outputs", show (map fromVariable outp)),
                    ("inputs" , show (map fromVariable inp))] >>=                               
    throwIfErrorIO0
  
{# fun unsafe arbb_op as op'
   { fromFunction `Function' ,
     cFromEnum `Opcode'  , 
     withVariableArray* `[Variable]' ,
     withVariableArray* `[Variable]' , 
     id `Ptr (Ptr ())'  ,
     id `Ptr (Ptr ())'  , 
     alloca- `ErrorDetails' peekErrorDet*  } -> `Error' cToEnum #} 
    -- alloca- `ErrorDetails' peekErrorDet* 

-- Operation that works on arrays of various length
-- TODO: Add Debug printing here
opDynamic fnt opc outp inp = 
    opDynamic' fnt 
               opc 
               nout 
               outp 
               nin 
               inp 
               nullPtr 
               nullPtr >>= throwIfErrorIO0      
   where 
     nin = length inp 
     nout = length outp  

opDynamicImm  opc outp inp = 
    opDynamic' (Function nullPtr) 
               opc 
               nout 
               outp 
               nin 
               inp 
               nullPtr 
               nullPtr >>= throwIfErrorIO0      
   where 
     nin = length inp 
     nout = length outp  

     
{# fun unsafe arbb_op_dynamic as opDynamic' 
   { fromFunction `Function' ,
     cFromEnum    `Opcode'   ,
     cIntConv     `Int'      , 
     withVariableArray* `[Variable]' ,
     cIntConv     `Int'      , 
     withVariableArray* `[Variable]' ,
     id `Ptr (Ptr ())' ,
     id `Ptr (Ptr ())' ,
     alloca- `ErrorDetails' peekErrorDet*  } ->  `Error' cToEnum #}

-- callOp can be used to map a function over an array 
callOp caller opc callee outp inp = 
  callOp' caller opc callee outp inp >>= 
   dbg0 "arbb_call_op" [("caller",show $ fromFunction caller),
                        ("Opcode", show opc),
                        ("callee", show $ fromFunction callee), 
                        ("outputs", show (map fromVariable outp)),
                        ("inputs" , show (map fromVariable inp))] >>=                               
   
  throwIfErrorIO0


{# fun unsafe arbb_call_op as callOp'
   { fromFunction `Function' ,
     cFromEnum    `CallOpcode' ,
     fromFunction `Function' ,
     withVariableArray* `[Variable]' ,
     withVariableArray* `[Variable]' , 
     alloca- `ErrorDetails' peekErrorDet* } -> `Error'  cToEnum #}

-- ----------------------------------------------------------------------
-- COMPILE AND RUN

-- execute f outp inp = execute' f outp inp nullPtr >>= \x -> throwIfErrorIO (x,())
execute f outp inp = 
   execute' f outp inp >>= 
    dbg0 "arbb_execute" [("fun",show $ fromFunction f),
                         ("outputs", show (map fromVariable outp)),
                         ("inputs" , show (map fromVariable inp))] >>=                               
   
   throwIfErrorIO0          
 
{# fun unsafe arbb_execute as execute' 
   { fromFunction `Function'   ,        
     withVariableArray* `[Variable]' ,
     withVariableArray* `[Variable]' , 
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}
--     alloca- `ErrorDetails' peekErrorDet*  


-- There is no more "compile" function in this way 
-- in the latest version of arbb_vmapi.h
{-
compile f = 
   compile' f >>= 
    dbg0 "arbb_compile" [("fun",show $ fromFunction f)] >>=                               
    throwIfErrorIO0

{# fun unsafe arbb_compile as compile' 
   { fromFunction `Function' ,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}
-} 
finish = finish' >>= throwIfErrorIO0
{# fun unsafe arbb_finish as finish' 
   {alloca- `ErrorDetails' peekErrorDet*} -> `Error' cToEnum #}

-- ----------------------------------------------------------------------
-- Variables, Constants ..


createConstant :: Context -> Type -> Ptr () -> IO GlobalVariable
createConstant ctx t d = 
   createConstant' ctx t d nullPtr >>= 
   dbg  "arbb_create_constant" [("context",show $ fromContext ctx),  
                                ("type",show $ fromType t),
                                ("dataPtr", show d)]  ("globalVar", fromGlobalVariable) >>=                               
   throwIfErrorIO1
  
{# fun unsafe arbb_create_constant as createConstant' 
   { fromContext `Context'  ,
     alloca- `GlobalVariable' peekGlobalVariable*  ,
     fromType `Type'   , 
     id      `Ptr ()' ,
     id      `Ptr ()' , 
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 

createLocal :: Function -> Type -> String -> IO Variable
createLocal fnt t name = 
  createLocal' fnt t name >>= 
  dbg  "arbb_create_local" [("fun",show $ fromFunction fnt),  
                            ("type",show $ fromType t),
                            ("name", name)]  ("variable", fromVariable) >>=                               
  throwIfErrorIO1
{# fun unsafe arbb_create_local as createLocal'
    { fromFunction `Function'  ,        
      alloca- `Variable' peekVariable*  ,
      fromType `Type' ,
      withCString* `String' ,
      alloca- `ErrorDetails' peekErrorDet*  } -> `Error' cToEnum #} 


variableFromGlobal :: Context -> GlobalVariable -> IO Variable
variableFromGlobal ctx g =
   variableFromGlobal' ctx g >>= 
   dbg  "arbb_get_variable_from_global" [("ctx",show $ fromContext ctx),  
                                         ("globVar",show $ fromGlobalVariable g)]    
                                         ("variable", fromVariable) >>= 
   throwIfErrorIO1

{# fun unsafe arbb_get_variable_from_global as variableFromGlobal'
   { fromContext `Context'   ,
     alloca- `Variable' peekVariable* ,
     fromGlobalVariable `GlobalVariable' ,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 


getParameter :: Function -> Int -> Int -> IO Variable
getParameter f n m = 
  getParameter' f n m >>= 
  dbg  "arbb_get_parameter" [("fun",show $ fromFunction f),  
                             ("in/out",show n),   
                             ("index", show m)] ("variable", fromVariable) >>= 
  throwIfErrorIO1
 
{# fun unsafe arbb_get_parameter as getParameter' 
   { fromFunction `Function'   ,
     alloca- `Variable' peekVariable* ,
     cIntConv `Int'  , 
     cIntConv `Int'  , 
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}

readScalar :: Context -> Variable -> Ptr () -> IO ()
readScalar ctx v ptr = 
   readScalar' ctx v ptr >>= 
   dbg0  "arbb_read_scalar" [("ctx",show $ fromContext ctx),  
                             ("variable",show $ fromVariable v),   
                             ("dataOutPtr", show ptr)] >>= 
   throwIfErrorIO0

{# fun unsafe arbb_read_scalar as readScalar' 
   { fromContext `Context'  ,
     fromVariable `Variable' , 
     id          `Ptr ()'   ,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}

writeScalar :: Context -> Variable -> Ptr () -> IO ()
writeScalar ctx v ptr = 
  writeScalar' ctx v ptr >>= 
  dbg0  "arbb_write_scalar" [("ctx",show $ fromContext ctx),  
                             ("variable",show $ fromVariable v),   
                             ("dataPtr", show ptr)] >>= 
  throwIfErrorIO0

{# fun unsafe arbb_write_scalar as writeScalar' 
   { fromContext `Context' ,
     fromVariable `Variable' , 
     id  `Ptr ()' ,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}   


serializeFunction :: Function -> IO VMString
serializeFunction fun = 
   serializeFunction' fun >>= throwIfErrorIO1

-- TODO: use finalizer to remove VMString ? (ForeignPtr)
{# fun unsafe arbb_serialize_function as serializeFunction'
   { fromFunction `Function'  , 
     alloca- `VMString' peekVMString*,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}
  
--void arbb_free_string(arbb_string_t string);
{# fun unsafe arbb_free_string as freeVMString
   { fromVMString `VMString'  } -> `()' #} 

--const char* arbb_get_c_string(arbb_string_t string);
{# fun pure arbb_get_c_string as getCString 
   { fromVMString `VMString' } -> `String' peekCString* #}

-- ----------------------------------------------------------------------
-- Flowcontrol



-- LOOPS 
beginLoop fnt kind = 
  beginLoop' fnt kind >>= 
  dbg0  "arbb_begin_loop" [("fun",show $ fromFunction fnt),  
                           ("kind",show kind) ]  >>= 
  throwIfErrorIO0

{# fun unsafe arbb_begin_loop as beginLoop' 
   { fromFunction `Function' ,
     cFromEnum `LoopType'    ,
     alloca- `ErrorDetails' peekErrorDet*  } ->  `Error' cToEnum #}

beginLoopBlock fnt block =           
  beginLoopBlock' fnt block >>= 
  dbg0  "arbb_begin_loop_block" [("fun",show $ fromFunction fnt),  
                                 ("block",show block) ]  >>= 
  throwIfErrorIO0 

{# fun unsafe arbb_begin_loop_block as beginLoopBlock'
   { fromFunction `Function'   ,
     cFromEnum    `LoopBlock'  ,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 


loopCondition fnt var = 
  loopCondition' fnt var >>= 
  dbg0  "arbb_loop_condition" [("fun",show $ fromFunction fnt),  
                               ("condVar",show $ fromVariable var) ]  >>= 
  throwIfErrorIO0

{#fun unsafe arbb_loop_condition as loopCondition'
      { fromFunction `Function'   , 
        fromVariable `Variable'   , 
        alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}




endLoop fnt = 
    endLoop' fnt >>= 
    dbg0  "arbb_end_loop" [("fun",show $ fromFunction fnt)]  >>= 
    throwIfErrorIO0
 
{#fun unsafe arbb_end_loop as endLoop' 
      { fromFunction `Function' ,
        alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 


break fnt = 
   break' fnt  >>= 
   dbg0  "arbb_break" [("fun",show $ fromFunction fnt)]  >>= 
   throwIfErrorIO0

{#fun unsafe arbb_break as break' 
      { fromFunction `Function' ,
        alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 

continue fnt = 
  continue' fnt >>= 
   dbg0  "arbb_continue" [("fun",show $ fromFunction fnt)]  >>= 
   throwIfErrorIO0

{#fun unsafe arbb_continue as continue' 
      { fromFunction `Function' ,
        alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 



-- if then else 

ifBranch f v = ifBranch' f v >>= throwIfErrorIO0

{# fun unsafe arbb_if as ifBranch' 
   { fromFunction `Function' ,
     fromVariable `Variable' ,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}



elseBranch f = elseBranch' f >>= throwIfErrorIO0
 
{#fun unsafe arbb_else as elseBranch' 
      { fromFunction `Function' ,
        alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 


endIf f = endIf' f >>= throwIfErrorIO0

{#fun unsafe arbb_end_if as endIf' 
      { fromFunction `Function' ,
        alloca- `ErrorDetails' peekErrorDet*} -> `Error' cToEnum #} 



-- ----------------------------------------------------------------------
-- Alternative means of data movement 

--mapToHost ctx var pitch mode = 
--   mapToHost' ctx var pitch mode >>= 

mapToHost ctx var mode = 
   mapToHost' ctx var mode >>= 
--  dbg "map_to_host" [("ctx",show $ fromContext ctx),
--                                ("variable" ,show (fromVariable var)),
--                                ("pitch", show pitch)]
--                               (-- something about the result -- ) >>=      
    throwIfErrorIO2

{# fun unsafe arbb_map_to_host as mapToHost'
   { fromContext  `Context'     ,
     fromVariable `Variable'    , 
     alloca- `Ptr ()' peek*     , 
     alloc3- `[Word64]' peek3*    ,
     cFromEnum `RangeAccessMode' ,
     alloca- `ErrorDetails' peekErrorDet*  } -> `Error' cToEnum #} 




-- ----------------------------------------------------------------------
-- Aux functions 

setNumThreads c w = setNumThreads' c w >>= throwIfErrorIO0 
setDecompDegree c w = setDecompDegree' c w >>= throwIfErrorIO0 
setHeapSize w1 w2 = setHeapSize' w1 w2 >>= throwIfErrorIO0

{# fun unsafe arbb_set_num_threads as setNumThreads' 
   { fromContext `Context'   ,
     cIntConv    `Word'      ,
     alloca-     `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 

 
{# fun unsafe arbb_set_decomp_degree as setDecompDegree'
   { fromContext `Context'  , 
     cIntConv    `Word'     , 
     alloca-     `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 

{# fun unsafe arbb_set_heap_size as setHeapSize' 
   { cIntConv    `Word64'   ,
     cIntConv    `Word64'   , 
     alloca-     `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 
 