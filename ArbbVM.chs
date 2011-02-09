{-# LANGUAGE ForeignFunctionInterface #-}

module ArbbVM where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

import Control.Applicative
import Control.Monad
import C2HS

-- #include "/opt/intel/arbb/1.0.0.013/include/arbb_vmapi.h"

#include <arbb_vmapi.h>
#include "arbb_vmwrap.h"

-- TODO: Make these newtype 
type Context = Ptr ()
type Type    = Ptr () 


newtype TypeStruct = TypeStruct (Ptr ())
{# pointer *arbb_type_t as TypeArray -> TypeStruct#}

{-
instance Storable StructName where
  sizeOf _ = {#sizeof StructName #}
  alignment _ = 4
  peek p = StructName
    <$> liftM cIntConv ({#get StructName->struct_field1 #} p)
    <*> liftM cIntConv ({#get StructName->struct_field2 #} p)
  poke p x = do
    {#set StructName.struct_field1 #} p (cIntConv $ struct_field1'StructName x)
    {#set StructName.struct_field2 #} p (cIntConv $ struct_field2'StructName x)
-}
instance Storable TypeStruct where
  sizeOf _ = {#sizeof arbb_type_t #}
  alignment _ = 0
  peek p =  TypeStruct <$> liftM id ({#get arbb_type_t->ptr #} p)
  poke p (TypeStruct x) = do
    {#set arbb_type_t.ptr #} p x
 

--------------------------------------------------------------------------
-- 

{# enum arbb_scalar_type_t as ScalarType 
    {underscoreToCase} deriving (Show, Eq) #}

--------------------------------------------------------------------------

{# fun arbb_wrap_get_default_context as defaultContext   
    { }  -> `Context' id #}
 
{# fun arbb_wrap_get_scalar_type as getScalarType
   { id `Context', 
     cFromEnum `ScalarType' } -> `Type' id #}

--getFunctionType ctx outp inp =   
--    getFunctionType_ ctx outp inp (length outp) (length inp) 


{# fun arbb_wrap_get_binary_function_type as getBinFunctionType
   { id `Context',                 
     id `Type'   , 
     id `Type'   , 
     id `Type'   } -> `Type' id #}
   
-- TODO: Does not work
{# fun arbb_wrap_get_function_type as getFunctionType_ 
   { id `Context',
     id  `TypeArray' , 
     id  `TypeArray' , 
     cIntConv   `Int'   , 
     cIntConv   `Int'    } -> `Type' id #}

-------------------------------------------------------------------------
{# fun fun4 as ^ 
   { id `Context',
     id `Type'   , 
     id `Type'  } -> `()' #} 


{# fun fun3 as ^ 
   { id `Context',
     id `Type'    } -> `()' #} 

{# fun fun2 as ^ 
   { id `Context'  } -> `()' #} 

{# fun fun1 as ^  
  {  } -> `()'  #}


{- 
type ArBBContext = Ptr ()

{# pointer *arbb_context_t as ArBBContextPtr -> ArBBContext #}

type ArBBErrorDetails = Ptr ()
{# pointer *arbb_error_details_t as ArBBErrorDetailsPtr -> ArBBErrorDetails #}



--arbb_error_t arbb_get_default_context(arbb_context_t* out_context,
--                                      arbb_error_details_t* details);

{# fun arbb_get_default_context as arbbGetDefaultContext 
  { id `ArBBContextPtr', 
    id `ArBBErrorDetailsPtr'} -> `CInt' id  #}

-}