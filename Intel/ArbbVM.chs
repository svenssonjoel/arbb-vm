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
  
{- TODOs 

   DONE: Figure out the error_details_t thing.
      - Pass a null pointer in, and don't care about them.   
       
   
   
-}


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

data ArbbVMException = ArbbVMException Error
  deriving (Eq, Show, Typeable)


instance Exception ArbbVMException 


--throwIfError :: Error -> a -> a 
--throwIfError error_code a = 
--     if fromEnum error_code > 0 then throw (ArbbVMException error_code) 
--                                else a 

throwIfErrorIO  :: (Error,a) -> IO a 
throwIfErrorIO (error_code,a) = 
     if fromEnum error_code > 0 then throwIO (ArbbVMException error_code) 
                                else return a 

-- ----------------------------------------------------------------------
-- BINDINGS 
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- getDefaultContext 
-- Inputs: None
-- Outputs: The default context.

getDefaultContext :: IO Context
getDefaultContext = getDefaultContext' nullPtr >>= throwIfErrorIO

{# fun arbb_get_default_context as getDefaultContext' 
   { alloca- `Context' peekContext* , 
     id      `Ptr (Ptr ())'    } -> `Error' cToEnum #} 
    -- alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 


    

-- ----------------------------------------------------------------------
-- getScalarType. 

getScalarType :: Context -> ScalarType -> IO Type
getScalarType ctx st = getScalarType' ctx st (nullPtr) >>= throwIfErrorIO
   
{# fun arbb_get_scalar_type as getScalarType' 
   { fromContext `Context' , 
     alloca-     `Type' peekType* ,
     cFromEnum   `ScalarType' ,
     id `Ptr (Ptr ())' } -> `Error' cToEnum #}
    -- alloca-     `ErrorDetails' peekErrorDet*  
  
     

-- ----------------------------------------------------------------------
-- Error handling 

-- These are sort of unusable now, since the error_details_t field is never used 
-- TODO: Keep theese in case we add more detailed error handling. 

--ARBB_VM_EXPORT
--const char* arbb_get_error_message(arbb_error_details_t error_details);
{# fun arbb_get_error_message as getErrorMessage 
   { fromErrorDetails `ErrorDetails' } -> `String' #} 


--ARBB_VM_EXPORT
--arbb_error_t arbb_get_error_code(arbb_error_details_t error_details);
{# fun arbb_get_error_code as getErrorCode 
   { fromErrorDetails `ErrorDetails' } -> `Error' cToEnum #}



--ARBB_VM_EXPORT
--void arbb_free_error_details(arbb_error_details_t error_details);
{# fun arbb_free_error_details as freeErrorDetails 
   { fromErrorDetails `ErrorDetails' } -> `()' #}


-- ----------------------------------------------------------------------
-- sizeOf 

sizeOf ctx t = sizeOf' ctx t nullPtr >>= throwIfErrorIO
  
{# fun arbb_sizeof_type as sizeOf' 
   { fromContext `Context'    ,
     alloca-     `Word64' peekCULLong*    ,
     fromType    `Type'       , 
     id          `Ptr (Ptr ())' } -> `Error' cToEnum #}
  where peekCULLong x = 
           do 
            res <- peek x 
            return (fromIntegral res)

-- alloca-     `ErrorDetails' peekErrorDet*

-- ----------------------------------------------------------------------
-- getDenseType
getDenseType ctx t dim = getDenseType' ctx t dim nullPtr >>= throwIfErrorIO
            
{# fun arbb_get_dense_type as getDenseType' 
   { fromContext `Context'   ,
     alloca-     `Type'   peekType* ,
     fromType    `Type'      , 
     cIntConv    `Int'       ,
     id `Ptr (Ptr ())' } -> `Error' cToEnum #}  
--  alloca-     `ErrorDetails' peekErrorDet*
-- ----------------------------------------------------------------------
-- createGlobal 


createGlobal ctx t name b = 
   createGlobal' ctx t name b nullPtr nullPtr >>= throwIfErrorIO 
             
{# fun arbb_create_global as createGlobal'
   { fromContext  `Context'     ,
     alloca-      `GlobalVariable' peekGlobalVariable* , 
     fromType     `Type'        ,
     withCString* `String'      , 
     fromBinding  `Binding'     , 
     id           `Ptr ()'      ,
     id           `Ptr (Ptr ())' } -> `Error' cToEnum #} 
     

     --alloca-      `ErrorDetails' peekErrorDet* 
     
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
  createDenseBinding' ctx d dim sizes pitches nullPtr >>= throwIfErrorIO
           
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
     id `Ptr (Ptr ())' } -> `Error' cToEnum #}

--arbb_error_t arbb_free_binding(arbb_context_t context,
--                               arbb_binding_t binding,
--                               arbb_error_details_t* details);

freeBinding ctx bind = 
  freeBinding' ctx bind nullPtr >>= \x -> throwIfErrorIO (x,()) 

{# fun arbb_free_binding as freeBinding'
   { fromContext `Context'  ,
     fromBinding `Binding'  ,
     id `Ptr (Ptr ())'      } -> `Error' cToEnum #}

   

-- ----------------------------------------------------------------------
-- FUNCTIONS 


getFunctionType ctx outp inp = 
  do 
    let outlen = length outp
        inlen  = length inp
    getFunctionType' ctx outlen outp inlen inp nullPtr >>= throwIfErrorIO 
 
{# fun arbb_get_function_type as getFunctionType' 
   { fromContext `Context'     ,
     alloca- `Type' peekType*  , 
     cIntConv `Int'            , 
     withTypeArray* `[Type]'   , 
     cIntConv `Int'            , 
     withTypeArray* `[Type]'   ,
     id `Ptr (Ptr ())' } -> `Error' cToEnum #}
     --alloca- `ErrorDetails' peekErrorDet* 

beginFunction ctx t name remote = 
  beginFunction' ctx t name remote nullPtr >>= throwIfErrorIO 

{# fun arbb_begin_function as beginFunction'
   { fromContext `Context'   ,
     alloca- `Function' peekFunction* ,
     fromType `Type'   ,
     withCString* `String'  ,
     cIntConv     `Int'    ,
     id `Ptr (Ptr ())' } -> `Error' cToEnum #}
    -- alloca- `ErrorDetails' peekErrorDet* 

endFunction f = 
   endFunction' f nullPtr >>= (\x ->  throwIfErrorIO (x,()))   
 
{#fun arbb_end_function as endFunction' 
      { fromFunction `Function'    ,
        id `Ptr (Ptr ())'  } -> `Error' cToEnum #}
  ---alloca- `ErrorDetails' peekErrorDet*

-- ----------------------------------------------------------------------
-- Operations of various kinds

-- Operations on scalaras 
op f opcode outp inp = 
    op' f opcode outp inp nullPtr nullPtr nullPtr >>= \x -> throwIfErrorIO (x,())
  
{# fun arbb_op as op'
   { fromFunction `Function' ,
     cFromEnum `Opcode'  , 
     withVariableArray* `[Variable]' ,
     withVariableArray* `[Variable]' , 
     id `Ptr (Ptr ())'  ,
     id `Ptr (Ptr ())'  , 
     id `Ptr (Ptr ())' } -> `Error' cToEnum #} 
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
               nullPtr nullPtr >>= \x -> throwIfErrorIO (x,())      
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
     id `Ptr (Ptr ())' } ->  `Error' cToEnum #}

-- callOp can be used to map a function over an array 
callOp caller opc callee outp inp = 
  callOp' caller opc callee outp inp nullPtr >>= \x -> throwIfErrorIO (x,()) 

{# fun arbb_call_op as callOp'
   { fromFunction `Function' ,
     cFromEnum    `CallOpcode' ,
     fromFunction `Function' ,
     withVariableArray* `[Variable]' ,
     withVariableArray* `[Variable]' , 
     id `Ptr (Ptr ())'  } -> `Error'  cToEnum #}

-- ----------------------------------------------------------------------
-- COMPILE AND RUN

execute f outp inp = execute' f outp inp nullPtr >>= \x -> throwIfErrorIO (x,())

{# fun arbb_execute as execute' 
   { fromFunction `Function'   ,        
     withVariableArray* `[Variable]' ,
     withVariableArray* `[Variable]' , 
     id `Ptr (Ptr ())' } -> `Error' cToEnum #}
--     alloca- `ErrorDetails' peekErrorDet*  

compile f = compile' f nullPtr >>= \x -> throwIfErrorIO (x,())
  -- do 
  --  (error_code, erro_details) <- compile' f
   -- return ()

{# fun arbb_compile as compile' 
   { fromFunction `Function' ,
     id `Ptr (Ptr ())' } -> `Error' cToEnum #}
-- alloca- `ErrorDetails' peekErrorDet*


--arbb_error_t arbb_finish(arbb_error_details_t* details);
finish = finish' nullPtr >>= \x -> throwIfErrorIO (x,())
{# fun arbb_finish as finish' 
   { id `Ptr (Ptr ())'} -> `Error' cToEnum #}

-- ----------------------------------------------------------------------
-- Variables, Constants ..


-- createConstant
createConstant ctx t d = 
   createConstant' ctx t d nullPtr nullPtr >>= throwIfErrorIO
  
{# fun arbb_create_constant as createConstant' 
   { fromContext `Context'  ,
     alloca- `GlobalVariable' peekGlobalVariable*  ,
     fromType `Type'   , 
      id      `Ptr ()' ,
      id      `Ptr ()' , 
      id      `Ptr (Ptr ())' } -> `Error' cToEnum #} 

createLocal fnt t name = 
  createLocal' fnt t name nullPtr >>= throwIfErrorIO
{# fun arbb_create_local as createLocal'
    { fromFunction `Function'  ,        
      alloca- `Variable' peekVariable*  ,
      fromType `Type' ,
      withCString* `String' ,
      id `Ptr (Ptr ())'  } -> `Error' cToEnum #} 


-- variableFromGlobal
variableFromGlobal ctx g =
   variableFromGlobal' ctx g nullPtr >>= throwIfErrorIO 



{# fun arbb_get_variable_from_global as variableFromGlobal'
   { fromContext `Context'   ,
     alloca- `Variable' peekVariable* ,
     fromGlobalVariable `GlobalVariable' ,
     id `Ptr (Ptr ())' } -> `Error' cToEnum #} 
-- alloca- `ErrorDetails' peekErrorDet*

-- getParameter
getParameter f n m = 
  getParameter' f n m nullPtr >>= throwIfErrorIO
 
{# fun arbb_get_parameter as getParameter' 
   { fromFunction `Function'   ,
     alloca- `Variable' peekVariable* ,
     cIntConv `Int'  , 
     cIntConv `Int'  , 
     id `Ptr (Ptr ())' } -> `Error' cToEnum #}
--alloca- `ErrorDetails' peekErrorDet*

readScalar ctx v ptr = 
   readScalar' ctx v ptr nullPtr >>= \x -> throwIfErrorIO (x,())

{# fun arbb_read_scalar as readScalar' 
   { fromContext `Context'  ,
     fromVariable `Variable' , 
     id          `Ptr ()'   ,
     id `Ptr (Ptr ())'  } -> `Error' cToEnum #}
--alloca- `ErrorDetails' peekErrorDet*

writeScalar ctx v ptr = 
  writeScalar' ctx v ptr nullPtr >>= \x -> throwIfErrorIO (x,()) 

{# fun arbb_write_scalar as writeScalar' 
   { fromContext `Context' ,
     fromVariable `Variable' , 
     id  `Ptr ()' ,
     id  `Ptr (Ptr ())' } -> `Error' cToEnum #}   


--arbb_error_t arbb_serialize_function(arbb_function_t function,
--                                     arbb_string_t* out_text,
--                                     arbb_error_details_t* details);

serializeFunction fun = 
   serializeFunction' fun nullPtr >>= throwIfErrorIO 

-- TODO: use finalizer to remove VMString ? (ForeignPtr)
{# fun arbb_serialize_function as serializeFunction'
   { fromFunction `Function'  , 
     alloca- `VMString' peekVMString*,
     id `Ptr (Ptr ())' } -> `Error' cToEnum #}
  
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
  beginLoop' fnt kind nullPtr >>= \x -> throwIfErrorIO (x,())

{# fun arbb_begin_loop as beginLoop' 
   { fromFunction `Function' ,
     cFromEnum `LoopType'    ,
     id `Ptr (Ptr ())'  } ->  `Error' cToEnum #}

beginLoopBlock fnt block =           
  beginLoopBlock' fnt block nullPtr >>= \x -> throwIfErrorIO (x,()) 

{# fun arbb_begin_loop_block as beginLoopBlock'
   { fromFunction `Function'   ,
     cFromEnum    `LoopBlock'  ,
     id `Ptr (Ptr ())'        } -> `Error' cToEnum #} 


loopCondition fnt var = 
  loopCondition' fnt var nullPtr  >>= \x -> throwIfErrorIO (x,()) 

{#fun arbb_loop_condition as loopCondition'
      { fromFunction `Function'   , 
        fromVariable `Variable'   , 
        id `Ptr (Ptr ())'         } -> `Error' cToEnum #}




endLoop fnt = endLoop' fnt nullPtr >>= \x -> throwIfErrorIO (x,()) 
 
{#fun arbb_end_loop as endLoop' 
      { fromFunction `Function' ,
        id `Ptr (Ptr ())' } -> `Error' cToEnum #} 


break fnt = break' fnt nullPtr >>= \x -> throwIfErrorIO (x,())

{#fun arbb_break as break' 
      { fromFunction `Function' ,
        id `Ptr (Ptr ())' } -> `Error' cToEnum #} 

continue fnt = continue' fnt nullPtr >>= \x -> throwIfErrorIO (x,())

{#fun arbb_continue as continue' 
      { fromFunction `Function' ,
        id `Ptr (Ptr ())' } -> `Error' cToEnum #} 



-- if then else 

ifBranch f v = ifBranch' f v nullPtr >>= \x -> throwIfErrorIO (x,())

{# fun arbb_if as ifBranch' 
   { fromFunction `Function' ,
     fromVariable `Variable' ,
     id `Ptr (Ptr ())'  } -> `Error' cToEnum #}



elseBranch f = elseBranch' f nullPtr >>= \x -> throwIfErrorIO (x,())
 
{#fun arbb_else as elseBranch' 
      { fromFunction `Function' ,
        id `Ptr (Ptr ())' } -> `Error' cToEnum #} 


endIf f = endIf' f nullPtr >>= \x -> throwIfErrorIO (x,())

{#fun arbb_end_if as endIf' 
      { fromFunction `Function' ,
        id `Ptr (Ptr ())' } -> `Error' cToEnum #} 



-- ----------------------------------------------------------------------
-- Alternative means of data movement 

mapToHost ctx var pitch mode = 
   mapToHost' ctx var pitch mode nullPtr >>= throwIfErrorIO

{# fun arbb_map_to_host as mapToHost'
   { fromContext  `Context'     ,
     fromVariable `Variable'    , 
     alloca- `Ptr ()' peek*     , 
     withIntArray* `[Word64]'    ,
     cFromEnum `RangeAccessMode' ,
     id `Ptr (Ptr ())'  } -> `Error' cToEnum #} 





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
