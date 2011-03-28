
module Data.Array.Accelerate.ArBB.State where 

import Intel.ArbbVM
import Intel.ArbbVM.Convenience hiding (liftIO)
import qualified Intel.ArbbVM.Convenience as ArBB

import Foreign.Ptr 

import qualified Control.Monad.State.Strict as S 
import qualified Data.Map as M

------------------------------------------------------------------------------
-- Extra level of statemonad wrapped around our EmitArbb                                                                              

type ArBBVariableMap = M.Map (Ptr ()) Variable 

type ExecState a = S.StateT ArBBVariableMap EmitArbb a 

runExecState :: ExecState a -> EmitArbb (a, ArBBVariableMap)
runExecState a = S.runStateT a M.empty

liftArBB :: EmitArbb a -> ExecState a
liftArBB = S.lift 

liftIO :: IO a -> ExecState a 
liftIO = S.lift .  ArBB.liftIO


------------------------------------------------------------------------------