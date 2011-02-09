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
 

--------------------------------------------------------------------------
-- 

{# enum arbb_scalar_type_t as ScalarType 
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
   
-- TODO: Does this REALLY work ?
{# fun arbb_wrap_get_function_type as getFunctionType 
   { fromContext `Context',
     withArray*  `[Type]' , 
     withArray*  `[Type]' , 
     cIntConv   `Int'   , 
     cIntConv   `Int'    } -> `Type' Type #}


-------------------------------------------------------------------------
{# fun fun4 as ^ 
   { fromContext `Context',
     fromType `Type'   , 
     fromType `Type'  } -> `()' #} 


{# fun fun3 as ^ 
   { fromContext `Context',
     fromType `Type'    } -> `()' #} 

{# fun fun2 as ^ 
   { fromContext `Context'  } -> `()' #} 

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