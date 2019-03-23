module Sphere
    ( Sphere (..)
    ) where

import Matrix (Matrix4)

data Sphere a = Sphere (Matrix4 a) deriving (Eq, Show)
