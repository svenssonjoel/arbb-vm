{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS  -XDeriveDataTypeable #-}

module Intel.ArbbVM where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

import Control.Applicative
import Control.Monad
import Control.Exception

import Data.Typeable

import C2HS


#include "../cbits/arbb_alt.h"
  
{- TODOs 

   Figure out the error_details_t thing.
    - Pass a null pointer in, and don't care about them.   
       

   
-}


-- ----------------------------------------------------------------------

newtype Context = Context {fromContext :: Ptr ()} 

{-
instance Storable Context where
  sizeOf _ = {#sizeof arbb_context_t #}
  alignment _ = 4
  peek p =  peek (castPtr p)   
      
  poke p x = poke (castPtr p) x
-}

newtype ErrorDetails = ErrorDetails {fromErrorDetails :: Ptr ()} 
newtype Type = Type {fromType :: Ptr ()} 
newtype Variable = Variable {fromVariable :: Ptr ()}
newtype GlobalVariable = GlobalVariable {fromGlobalVariable :: Ptr ()}
newtype Binding = Binding {fromBinding :: Ptr ()}
newtype Function = Function {fromFunction :: Ptr ()}
newtype VMString = VMString {fromVMString :: Ptr ()}

-- ----------------------------------------------------------------------
-- ENUMS
-- ----------------------------------------------------------------------
{# enum arbb_error_t as Error 
   {underscoreToCase} deriving (Show, Eq) #}

{# enum arbb_scalar_type_t as ScalarType 
   {underscoreToCase} deriving (Show, Eq) #}

{# enum arbb_opcode_t as Opcode 
   {underscoreToCase} deriving (Show, Eq) #}

{# enum arbb_call_opcode_t as CallOpCode 
   {underscoreToCase} deriving (Show, Eq) #}

{# enum arbb_loop_type_t as LoopType 
   {underscoreToCase} deriving (Show, Eq) #}

{# enum arbb_loop_block_t as LoopBlock
   {underscoreToCase} deriving (Show, Eq) #} 


-- ----------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------
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

getDefaultContext = getDefaultContext' nullPtr >>= throwIfErrorIO

{# fun arbb_get_default_context as getDefaultContext' 
   { alloca- `Context' peekContext* , 
     id      `Ptr (Ptr ())'    } -> `Error' cToEnum #} 
    -- alloca- `ErrorDetails' peekErrorDet* } -> `Error' cToEnum #} 


    

-- ----------------------------------------------------------------------
-- getScalarType. 

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
-- TODO: REMOVE THESE 

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
     alloca-     `Integer' peekCULLong*    ,
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

{# fun arbb_set_binding_null as getBindingNull 
   { alloca- `Binding' peekBinding*  } -> `()'#} 
  


createDenseBinding ctx d dim sizes pitches = 
  createDenseBinding' ctx d dim sizes' pitches' nullPtr >>= throwIfErrorIO
 where
   sizes' = map fromIntegral sizes
   pitches' = map fromIntegral pitches
           
{# fun arbb_create_dense_binding as createDenseBinding'  
   { fromContext `Context'  ,
     alloca- `Binding' peekBinding* ,
     id `Ptr ()' ,
     cIntConv `Int' ,
     withArray* `[CULLong]',
     withArray* `[CULLong]', 
     id `Ptr (Ptr ())' } -> `Error' cToEnum #}


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
-- op


op f opcode outp inp = 
    op' f opcode outp inp nullPtr nullPtr >>= \x -> throwIfErrorIO (x,())
  
{# fun arbb_op as op'
   { fromFunction `Function' ,
     cFromEnum `Opcode'  , 
     withVariableArray* `[Variable]' ,
     withVariableArray* `[Variable]' , 
     id `Ptr (Ptr ())'  ,
     id `Ptr (Ptr ())' } -> `Error' cToEnum #} 
    -- alloca- `ErrorDetails' peekErrorDet* 


--arbb_error_t arbb_op_dynamic(arbb_function_t function,
--                             arbb_opcode_t opcode,
--                             unsigned int num_outputs,
--                             const arbb_variable_t* outputs,
--                             unsigned int num_inputs,
--                             const arbb_variable_t* inputs,
--                             void* debug_data_ptrs[],
--                             arbb_error_details_t* details);
opDynamic fnt opc outp inp = 
    opDynamic' fnt opc nout outp nin inp nullPtr nullPtr >>= \x -> throwIfErrorIO (x,())      
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
     id `Ptr (Ptr ())' } ->  `Error' cToEnum #}


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
  


--const char* arbb_get_c_string(arbb_string_t string);
{# fun pure arbb_get_c_string as getCString 
   { fromVMString `VMString' } -> `String' peekCString* #}

-- ----------------------------------------------------------------------
-- Flowcontrol



-- LOOPS 

--arbb_error_t arbb_begin_loop(arbb_function_t function,
--                             arbb_loop_type_t loop_type,
--                             arbb_error_details_t* details);

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

