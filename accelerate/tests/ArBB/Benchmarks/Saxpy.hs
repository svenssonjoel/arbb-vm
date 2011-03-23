

module Saxpy where 

import Data.Array.Accelerate as Acc 

saxpyAcc :: Float -> Vector Float -> Vector Float -> Acc (Vector Float)
saxpyAcc alpha xs ys
  = let
      xs' = use xs
      ys' = use ys
    in
    Acc.zipWith (\x y -> constant alpha * x + y) xs' ys'
