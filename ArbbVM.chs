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

newtype Context = Context {fromContext :: Ptr ()} 

newtype Type = Type {fromType :: Ptr ()}
{# pointer *arbb_type_t as TypeArray -> Type#}

instance Storable Type where
  sizeOf _ = {#sizeof arbb_type_t #}
  alignment _ = 0
  peek p =  Type <$> liftM id ({#get arbb_type_t->ptr #} p)
  poke p (Type x) = do
    {#set arbb_type_t.ptr #} p x


newtype Function = Function {fromFunction :: Ptr ()}


newtype Variable = Variable {fromVariable :: Ptr ()}
{# pointer *arbb_variable_t as VariableArray -> Variable#}

instance Storable Variable where
  sizeOf _ = {#sizeof arbb_type_t #}
  alignment _ = 0
  peek p =  Variable <$> liftM id ({#get arbb_type_t->ptr #} p)
  poke p (Variable x) = do
    {#set arbb_type_t.ptr #} p x


newtype Binding = Binding { fromBinding :: Ptr ()}


newtype GlobalVariable = GlobalVariable {fromGlobalVariable :: Ptr ()}



-------------------------------------------------------------------------- 

{# enum arbb_scalar_type_t as ScalarType 
    {underscoreToCase} deriving (Show, Eq) #}



{# enum arbb_opcode_t as Opcode 
    {underscoreToCase} deriving (Show, Eq) #}


--------------------------------------------------------------------------

{# fun arbb_wrap_get_default_context as defaultContext   
    { }  -> `Context' Context #}
 
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
-- TODO: Does this REALLY work ?
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
-------------------------------------------------------------------------
