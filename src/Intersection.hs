module Intersection
    ( Intersection (..)
    , intersect
    , hit
    ) where

import Data.List (sortBy)

import Ray (Ray (..), transform)
import Sphere (Sphere (..))
import Matrix (point, multTuple4, dotTuple4, inverse)

data Intersection a = Intersection a (Sphere a) deriving (Eq, Show)

intersect :: (Ord a, Floating a) => Sphere a -> Ray a -> [Intersection a]
intersect s@(Sphere tMatrix) ray =
    let
        Ray origin direction = transform (inverse tMatrix) ray
        sphereToRay = origin - point 0 0 0
        a = dotTuple4 direction direction
        b = 2 * dotTuple4 direction sphereToRay
        c = dotTuple4 sphereToRay sphereToRay - 1
        discriminant = b^2 - 4 * a * c
    in
        if discriminant < 0 then
            []
        else
            [ Intersection (((-b) - sqrt discriminant) / (2 * a)) s
            , Intersection (((-b) + sqrt discriminant) / (2 * a)) s
            ]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (h:_) = Just h

isPositiveT :: (Ord a, Num a) => Intersection a -> Bool
isPositiveT (Intersection t _) = t > 0

sortIntersections :: (Ord a) => Intersection a -> Intersection a -> Ordering
sortIntersections (Intersection t1 _) (Intersection t2 _) =
    compare t1 t2

hit :: (Ord a, Floating a) => [Intersection a] -> Maybe (Intersection a)
hit = safeHead . filter isPositiveT . sortBy sortIntersections
