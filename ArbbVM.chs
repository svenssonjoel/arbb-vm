{-# LANGUAGE ForeignFunctionInterface #-}

module ArbbVM where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

import Control.Applicative
import Control.Monad
import C2HS


#include <arbb_vmapi.h>
#include "arbb_vmwrap.h"

-- ----------------------------------------------------------------------

newtype Context = Context {fromContext :: Ptr ()} 
{# pointer *arbb_context_t as PtrContext -> Context #}

instance Storable Context where
  sizeOf _ = {#sizeof arbb_context_t #}
  alignment _ = 4
  peek p =  Context <$> liftM id ({#get arbb_context_t->ptr #} p)
  poke p (Context x) = do
    {#set arbb_context_t.ptr #} p x


newtype Type = Type {fromType :: Ptr ()}
{# pointer *arbb_type_t as PtrType -> Type#}

instance Storable Type where
  sizeOf _ = {#sizeof arbb_type_t #}
  alignment _ = 4
  peek p =  Type <$> liftM id ({#get arbb_type_t->ptr #} p)
  poke p (Type x) = do
    {#set arbb_type_t.ptr #} p x


newtype Function = Function {fromFunction :: Ptr ()}

newtype Variable = Variable {fromVariable :: Ptr ()}
{# pointer *arbb_variable_t as PtrVariable -> Variable#}

instance Storable Variable where
  sizeOf _ = {#sizeof arbb_type_t #}
  alignment _ = 0
  peek p =  Variable <$> liftM id ({#get arbb_type_t->ptr #} p)
  poke p (Variable x) = do
    {#set arbb_type_t.ptr #} p x


newtype Binding = Binding { fromBinding :: Ptr ()}

newtype Serialized = Serialized { fromSerialized :: Ptr ()}

newtype GlobalVariable = GlobalVariable {fromGlobalVariable :: Ptr ()}



-------------------------------------------------------------------------- 

{# enum arbb_scalar_type_t as ScalarType 
    {underscoreToCase} deriving (Show, Eq) #}



{# enum arbb_opcode_t as Opcode 
    {underscoreToCase} deriving (Show, Eq) #}


--------------------------------------------------------------------------


{# fun arbb_wrap_get_default_context as defaultContext   
    { }  -> `Context' Context #}


-------------------------------------------------------------------- 
--   try the 
--   withDefaultContext $ \ctx -> f ctx 
--   approach
--------------------------------------------------------------------

withDefaultContext :: (Context -> IO a) -> IO a 
withDefaultContext f = 
   do 
     alloca $ \ctx -> 
      do 
        getDef ctx nullPtr
        -- Todo: is this Ok?
        ctx' <- peek ctx    
        f ctx'
         
{# fun arbb_get_default_context as getDef 
   { id `Ptr Context' ,
     id `Ptr ()'  } -> `()' #} 

-- TODO: What is this arbb_error_details_t thing ? 
-- TODO: How take care of errors at all ? 

-- TODO: Apply this approach to other functions
--       May lead to not needing to wrap everything !

-- TODO: This "brackets" approach may work at other places too ?
-------------------------------------------------------------------- 

 
{# fun arbb_wrap_get_scalar_type as getScalarType
   { fromContext `Context', 
     cFromEnum `ScalarType' } -> `Type' Type #}

{# fun arbb_wrap_get_binary_function_type as getBinFunctionType
   { fromContext `Context',                 
     fromType `Type'   , 
     fromType `Type'   , 
     fromType `Type'   } -> `Type' Type #}
   

-- 
getFunctionType ctx xs ys = getFunctionType_ ctx xs ys (length xs) (length ys) 

{# fun arbb_wrap_get_function_type as getFunctionType_ 
   { fromContext `Context',
     withArray*  `[Type]' , 
     withArray*  `[Type]' , 
     cIntConv   `Int'   , 
     cIntConv   `Int'    } -> `Type' Type #}


{# fun arbb_wrap_begin_function as beginFunction
   { fromContext `Context' , 
     fromType    `Type'    , 
     withCString* `String'  } -> `Function' Function #}


{# fun arbb_wrap_get_parameter as getParameter
   { fromFunction `Function' ,
     cIntConv     `Int'      ,
     cIntConv     `Int'      } ->  `Variable' Variable #}


{# fun arbb_wrap_op as op
   { fromFunction `Function'  ,
     cFromEnum `Opcode'       , 
     withArray*  `[Variable]' , 
     withArray*  `[Variable]' } -> `()' #}   


{# fun arbb_wrap_end_function as endFunction
   { fromFunction `Function' } -> `()' #}

{# fun arbb_wrap_compile as compile
   { fromFunction `Function' } -> `()' #}

{# fun arbb_wrap_set_binding_null as getNullBinding
   {  } -> `Binding' Binding #}

{# fun arbb_wrap_create_constant as createConstant 
   { fromContext `Context' , 
     fromType    `Type'    , 
     id          `Ptr ()'  } -> `GlobalVariable' GlobalVariable #}

{# fun arbb_wrap_variable_from_global as variableFromGlobal
   { fromContext `Context' ,
     fromGlobalVariable `GlobalVariable' } -> `Variable' Variable #} 

{# fun arbb_wrap_create_global as createGlobal 
   { fromContext `Context' ,
     fromType    `Type'    , 
     withCString* `String'  , 
     fromBinding `Binding' } -> `GlobalVariable' GlobalVariable #}

{# fun arbb_wrap_execute as execute 
   { fromFunction `Function'   , 
     withArray*   `[Variable]' ,
     withArray*   `[Variable]' } -> `()' #}

{# fun arbb_wrap_read_scalar_float as readScalarFloat 
   { fromContext `Context'   ,
     fromVariable `Variable' } -> `Float' #}

serializeFunction fnt = 
   do 
     ser <- serializeFunction_ fnt
     txt <- serializedToString ser
     freeSerialized ser
     return txt
     
{# fun arbb_wrap_serialize_function as serializeFunction_ 
   { fromFunction `Function' } -> `Serialized' Serialized #}

{# fun arbb_wrap_get_c_string as serializedToString 
   { fromSerialized `Serialized' } -> `String' #}


{#fun arbb_wrap_free_string as freeSerialized 
   { fromSerialized `Serialized' } -> `()' #}
-------------------------------------------------------------------------
