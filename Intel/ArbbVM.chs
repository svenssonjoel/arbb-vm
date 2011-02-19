{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS  -XDeriveDataTypeable #-}

module Intel.ArbbVM where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

import Control.Exception

import Data.Typeable
import Data.Word

import C2HS 


#include "../cbits/arbb_alt.h"
-- ----------------------------------------------------------------------

newtype Context = Context {fromContext :: Ptr ()} 
newtype ErrorDetails = ErrorDetails {fromErrorDetails :: Ptr ()} 
newtype Type = Type {fromType :: Ptr ()} 
newtype Variable = Variable {fromVariable :: Ptr ()}
newtype GlobalVariable = GlobalVariable {fromGlobalVariable :: Ptr ()}
newtype Binding = Binding {fromBinding :: Ptr ()}
newtype Function = Function {fromFunction :: Ptr ()}
newtype VMString = VMString {fromVMString :: Ptr ()}

-- types to support aux functionality
newtype AttributeMap = AttributeMap {fromAttributeMap :: Ptr ()}
-- TODO: there is a struct called arbb_attribute_key_value_t 
--       that needs to be handeld.

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

withTypeArray = withArray . (fmap fromType) 
withVariableArray = withArray . (fmap fromVariable) 
withIntArray xs = withArray (fmap fromIntegral xs)

-- ----------------------------------------------------------------------
-- Exception
-- ----------------------------------------------------------------------

data ArbbVMException = ArbbVMException Error String
  deriving (Eq, Show, Typeable)


instance Exception ArbbVMException 


--throwIfError :: Error -> a -> a 
--throwIfError error_code a = 
--     if fromEnum error_code > 0 then throw (ArbbVMException error_code) 
--                                else a 

-- TODO: Phase out 
throwIfErrorIO  :: (Error,a) -> IO a 
throwIfErrorIO (error_code,a) = 
     if fromEnum error_code > 0 then throwIO (ArbbVMException error_code "") 
                                else return a 

-- TODO: phase in
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
-- getDefaultContext 
-- Inputs: None
-- Outputs: The default context.

getDefaultContext :: IO Context
getDefaultContext = getDefaultContext' >>= throwIfErrorIO1

{# fun arbb_get_default_context as getDefaultContext' 
   { alloca- `Context' peekContext* , 
     alloca- `ErrorDetails' peekErrorDet*   } -> `Error' cToEnum #} 
    -- id      `Ptr (Ptr ())'  } -> `Error' cToEnum #} 


    

-- ----------------------------------------------------------------------
-- getScalarType. 

getScalarType :: Context -> ScalarType -> IO Type
getScalarType ctx st = getScalarType' ctx st >>= throwIfErrorIO1
   
{# fun arbb_get_scalar_type as getScalarType' 
   { fromContext `Context' , 
     alloca-     `Type' peekType* ,
     cFromEnum   `ScalarType' ,
     alloca-     `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}
    -- id `Ptr (Ptr ())'  
  
     

-- ----------------------------------------------------------------------
-- Error handling 

   
{# fun arbb_get_error_message as getErrorMessage 
   { fromErrorDetails `ErrorDetails' } -> `String' #} 


{# fun arbb_get_error_code as getErrorCode 
   { fromErrorDetails `ErrorDetails' } -> `Error' cToEnum #}


{# fun arbb_free_error_details as freeErrorDetails 
   { fromErrorDetails `ErrorDetails' } -> `()' #}


-- ----------------------------------------------------------------------
-- sizeOf 

sizeOf ctx t = sizeOf' ctx t >>= throwIfErrorIO1
  
{# fun arbb_sizeof_type as sizeOf' 
   { fromContext `Context'    ,
     alloca-     `Word64' peekCULLong*    ,
     fromType    `Type'       , 
     alloca-     `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}
  where peekCULLong x = 
           do 
            res <- peek x 
            return (fromIntegral res)

-- id          `Ptr (Ptr ())'

-- ----------------------------------------------------------------------
-- getDenseType
getDenseType ctx t dim = getDenseType' ctx t dim >>= throwIfErrorIO1
            
{# fun arbb_get_dense_type as getDenseType' 
   { fromContext `Context'   ,
     alloca-     `Type'   peekType* ,
     fromType    `Type'      , 
     cIntConv    `Int'       ,
     alloca-     `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}  
--  id `Ptr (Ptr ())'
-- ----------------------------------------------------------------------
-- createGlobal 

createGlobal :: Context -> Type -> String -> Binding -> IO GlobalVariable
createGlobal ctx t name b = 
   createGlobal' ctx t name b nullPtr >>= throwIfErrorIO1 
             
{# fun arbb_create_global as createGlobal'
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

{# fun pure arbb_is_binding_null as isBindingNull
   { fromBinding `Binding' } -> `Bool'  cToBool #} 

-- TODO: see if this needs to be done differently
{# fun arbb_set_binding_null as getBindingNull 
   { alloca- `Binding' peekBinding*  } -> `()'#} 
  

--createDenseBinding ::  Context -> Ptr () -> Word -> [CULLong] -> [CULLong] ->  IO Binding
--createDenseBinding ::  Context -> Ptr () -> Word -> [Integer] -> [Integer] ->  IO Binding
createDenseBinding ::  Context -> Ptr () -> Word -> [Word64] -> [Word64] ->  IO Binding
createDenseBinding ctx d dim sizes pitches = 
  createDenseBinding' ctx d dim sizes pitches >>= throwIfErrorIO1
           
{# fun arbb_create_dense_binding as createDenseBinding'  
   { fromContext `Context'  ,
     alloca- `Binding' peekBinding* ,
     id `Ptr ()' ,
--     cIntConv `Int' ,
     cIntConv `Word' ,
--     withCULArray* `[Integer]',
--     withCULArray* `[Integer]', 
     withIntArray* `[Word64]',
     withIntArray* `[Word64]', 
     alloca-      `ErrorDetails' peekErrorDet*  } -> `Error' cToEnum #}


freeBinding ctx bind = 
  freeBinding' ctx bind >>= throwIfErrorIO0 

{# fun arbb_free_binding as freeBinding'
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
    getFunctionType' ctx outlen outp inlen inp >>= throwIfErrorIO1 
 
{# fun arbb_get_function_type as getFunctionType' 
   { fromContext `Context'     ,
     alloca- `Type' peekType*  , 
     cIntConv `Int'            , 
     withTypeArray* `[Type]'   , 
     cIntConv `Int'            , 
     withTypeArray* `[Type]'   ,
     alloca-      `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}
     --id `Ptr (Ptr ())'

beginFunction :: Context -> Type -> String -> Int -> IO Function
beginFunction ctx t name remote = 
  beginFunction' ctx t name remote >>= throwIfErrorIO1 

{# fun arbb_begin_function as beginFunction'
   { fromContext `Context'   ,
     alloca- `Function' peekFunction* ,
     fromType `Type'   ,
     withCString* `String'  ,
     cIntConv     `Int'    ,
     alloca-      `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}
     -- alloca- `ErrorDetails' peekErrorDet* 

endFunction :: Function -> IO ()
endFunction f = 
   endFunction' f  >>= throwIfErrorIO0
 
{#fun arbb_end_function as endFunction' 
      { fromFunction `Function'    ,
        alloca- `ErrorDetails' peekErrorDet*  } -> `Error' cToEnum #}
  ---id `Ptr (Ptr ())'

-- ----------------------------------------------------------------------
-- Operations of various kinds

-- Operations on scalaras 
op :: Function -> Opcode -> [Variable] -> [Variable] -> IO ()
op f opcode outp inp = 
    op' f opcode outp inp nullPtr nullPtr >>= throwIfErrorIO0
  
{# fun arbb_op as op'
   { fromFunction `Function' ,
     cFromEnum `Opcode'  , 
     withVariableArray* `[Variable]' ,
     withVariableArray* `[Variable]' , 
     id `Ptr (Ptr ())'  ,
     id `Ptr (Ptr ())'  , 
     alloca- `ErrorDetails' peekErrorDet*  } -> `Error' cToEnum #} 
    -- alloca- `ErrorDetails' peekErrorDet* 

-- Operation that works on arrays of various length
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
     
{# fun arbb_op_dynamic as opDynamic' 
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
  callOp' caller opc callee outp inp >>= throwIfErrorIO0

{# fun arbb_call_op as callOp'
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
   execute' f outp inp >>= throwIfErrorIO0          
 
{# fun arbb_execute as execute' 
   { fromFunction `Function'   ,        
     withVariableArray* `[Variable]' ,
     withVariableArray* `[Variable]' , 
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}
--     alloca- `ErrorDetails' peekErrorDet*  

compile f = compile' f >>= throwIfErrorIO0

{# fun arbb_compile as compile' 
   { fromFunction `Function' ,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}

finish = finish' >>= throwIfErrorIO0
{# fun arbb_finish as finish' 
   {alloca- `ErrorDetails' peekErrorDet*} -> `Error' cToEnum #}

-- ----------------------------------------------------------------------
-- Variables, Constants ..


-- createConstant
createConstant ctx t d = 
   createConstant' ctx t d nullPtr >>= throwIfErrorIO1
  
{# fun arbb_create_constant as createConstant' 
   { fromContext `Context'  ,
     alloca- `GlobalVariable' peekGlobalVariable*  ,
     fromType `Type'   , 
     id      `Ptr ()' ,
     id      `Ptr ()' , 
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 

createLocal fnt t name = 
  createLocal' fnt t name >>= throwIfErrorIO1
{# fun arbb_create_local as createLocal'
    { fromFunction `Function'  ,        
      alloca- `Variable' peekVariable*  ,
      fromType `Type' ,
      withCString* `String' ,
      alloca- `ErrorDetails' peekErrorDet*  } -> `Error' cToEnum #} 


-- variableFromGlobal
variableFromGlobal ctx g =
   variableFromGlobal' ctx g >>= throwIfErrorIO1

{# fun arbb_get_variable_from_global as variableFromGlobal'
   { fromContext `Context'   ,
     alloca- `Variable' peekVariable* ,
     fromGlobalVariable `GlobalVariable' ,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 


-- getParameter
getParameter f n m = 
  getParameter' f n m >>= throwIfErrorIO1
 
{# fun arbb_get_parameter as getParameter' 
   { fromFunction `Function'   ,
     alloca- `Variable' peekVariable* ,
     cIntConv `Int'  , 
     cIntConv `Int'  , 
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}

readScalar ctx v ptr = 
   readScalar' ctx v ptr >>= throwIfErrorIO0

{# fun arbb_read_scalar as readScalar' 
   { fromContext `Context'  ,
     fromVariable `Variable' , 
     id          `Ptr ()'   ,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}


writeScalar ctx v ptr = 
  writeScalar' ctx v ptr >>= throwIfErrorIO0

{# fun arbb_write_scalar as writeScalar' 
   { fromContext `Context' ,
     fromVariable `Variable' , 
     id  `Ptr ()' ,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}   


--arbb_error_t arbb_serialize_function(arbb_function_t function,
--                                     arbb_string_t* out_text,
--                                     arbb_error_details_t* details);

serializeFunction fun = 
   serializeFunction' fun >>= throwIfErrorIO1

-- TODO: use finalizer to remove VMString ? (ForeignPtr)
{# fun arbb_serialize_function as serializeFunction'
   { fromFunction `Function'  , 
     alloca- `VMString' peekVMString*,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}
  
--void arbb_free_string(arbb_string_t string);
{# fun arbb_free_string as freeVMString
   { fromVMString `VMString'  } -> `()' #} 

--const char* arbb_get_c_string(arbb_string_t string);
{# fun pure arbb_get_c_string as getCString 
   { fromVMString `VMString' } -> `String' peekCString* #}

-- ----------------------------------------------------------------------
-- Flowcontrol



-- LOOPS 
beginLoop fnt kind = 
  beginLoop' fnt kind >>= throwIfErrorIO0

{# fun arbb_begin_loop as beginLoop' 
   { fromFunction `Function' ,
     cFromEnum `LoopType'    ,
     alloca- `ErrorDetails' peekErrorDet*  } ->  `Error' cToEnum #}

beginLoopBlock fnt block =           
  beginLoopBlock' fnt block >>= throwIfErrorIO0 

{# fun arbb_begin_loop_block as beginLoopBlock'
   { fromFunction `Function'   ,
     cFromEnum    `LoopBlock'  ,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 


loopCondition fnt var = 
  loopCondition' fnt var >>= throwIfErrorIO0

{#fun arbb_loop_condition as loopCondition'
      { fromFunction `Function'   , 
        fromVariable `Variable'   , 
        alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}




endLoop fnt = endLoop' fnt >>= throwIfErrorIO0
 
{#fun arbb_end_loop as endLoop' 
      { fromFunction `Function' ,
        alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 


break fnt = break' fnt  >>= throwIfErrorIO0

{#fun arbb_break as break' 
      { fromFunction `Function' ,
        alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 

continue fnt = continue' fnt >>= throwIfErrorIO0

{#fun arbb_continue as continue' 
      { fromFunction `Function' ,
        alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 



-- if then else 

ifBranch f v = ifBranch' f v >>= throwIfErrorIO0

{# fun arbb_if as ifBranch' 
   { fromFunction `Function' ,
     fromVariable `Variable' ,
     alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #}



elseBranch f = elseBranch' f >>= throwIfErrorIO0
 
{#fun arbb_else as elseBranch' 
      { fromFunction `Function' ,
        alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 


endIf f = endIf' f >>= throwIfErrorIO0

{#fun arbb_end_if as endIf' 
      { fromFunction `Function' ,
        alloca- `ErrorDetails' peekErrorDet*} -> `Error' cToEnum #} 



-- ----------------------------------------------------------------------
-- Alternative means of data movement 

mapToHost ctx var pitch mode = 
   mapToHost' ctx var pitch mode >>= throwIfErrorIO1

{# fun arbb_map_to_host as mapToHost'
   { fromContext  `Context'     ,
     fromVariable `Variable'    , 
     alloca- `Ptr ()' peek*     , 
     withIntArray* `[Word64]'    ,
     cFromEnum `RangeAccessMode' ,
     alloca- `ErrorDetails' peekErrorDet*  } -> `Error' cToEnum #} 





-- ----------------------------------------------------------------------
-- null and isThisNull ? 


-- int arbb_is_refcountable_null(arbb_refcountable_t object);


-- void arbb_set_refcountable_null(arbb_refcountable_t* object);


-- int arbb_is_error_details_null(arbb_error_details_t object);


-- void arbb_set_error_details_null(arbb_error_details_t* object);


-- int arbb_is_string_null(arbb_string_t object);


-- void arbb_set_string_null(arbb_string_t* object);

 
-- int arbb_is_context_null(arbb_context_t object);


-- void arbb_set_context_null(arbb_context_t* object);

-- int arbb_is_function_null(arbb_function_t object);


-- void arbb_set_function_null(arbb_function_t* object);


-- int arbb_is_variable_null(arbb_variable_t object);


-- void arbb_set_variable_null(arbb_variable_t* object);


-- int arbb_is_global_variable_null(arbb_global_variable_t object);


-- void arbb_set_global_variable_null(arbb_global_variable_t* object);


-- int arbb_is_binding_null(arbb_binding_t object);


-- void arbb_set_binding_null(arbb_binding_t* object);


-- int arbb_is_type_null(arbb_type_t object);


-- void arbb_set_type_null(arbb_type_t* object);

-- void arbb_cxx_set_stack_trace_null(arbb_cxx_stack_trace_t* object);


-- int arbb_cxx_is_stack_trace_null(arbb_cxx_stack_trace_t object);


-- void arbb_set_attribute_map_null(arbb_attribute_map_t* object);


-- int arbb_is_attribute_map_null(arbb_attribute_map_t object);

-- ----------------------------------------------------------------------
-- Auxiliary functionality
