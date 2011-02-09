{-# LANGUAGE ForeignFunctionInterface #-}

module ArbbVM where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

import C2HS

-- #include "/opt/intel/arbb/1.0.0.013/include/arbb_vmapi.h"

#include <arbb_vmapi.h>
#include "arbb_vmwrap.h"


type Context = Ptr ()
type Type    = Ptr () 

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
   
-- {# fun arbb_wrap_get_function_type as getFunctionType_ 
--   { id `Context',
--     withArray*   `[Type]' , 
--     withArray*   `[Type]' , 
--     cIntConv   `Int'   , 
--     cIntConv   `Int'    } -> `Type' id #}

-------------------------------------------------------------------------
{# fun fun4 as ^ 
   { id `Context',
     id `Type'   , 
     id `Type'    } -> `()' #} 


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