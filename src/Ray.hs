module Ray
    ( Ray (..)
    , position
    , transform
    ) where

import Matrix (Matrix4 (..), Tuple4 (..), multTuple4, multMatrixTuple4)

data Ray a = Ray (Tuple4 a) (Tuple4 a) deriving (Show, Eq)

rmap :: (Tuple4 a -> Tuple4 a) -> Ray a -> Ray a
rmap f (Ray origin direction) = Ray (f origin) (f direction)

position :: Num a => Ray a -> a -> Tuple4 a
position (Ray origin direction) = (origin +) . flip multTuple4 direction

transform :: (Num a) => Matrix4 a -> Ray a -> Ray a
transform = rmap . multMatrixTuple4
