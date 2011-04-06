
module UseNode where 


import Data.Array.Accelerate as Acc

useNodeAcc :: Vector Float -> Acc (Vector Float)
useNodeAcc xs = use xs
