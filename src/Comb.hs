module Comb
    ( starling
    , goldfinch
    , blackbird
    , dovekies
    , bunting
    ) where

starling :: (a -> b -> c) -> (a -> b) -> a -> c
starling f g x = f x $ g x

goldfinch :: (a -> b -> c) -> (d -> b) -> d -> a -> c
goldfinch a b c d = a d (b c)

blackbird :: (a -> b) -> (c -> d -> a) -> c -> d -> b
blackbird a b c d = a (b c d)

dovekies :: (a -> b -> c) -> (d -> a) -> d -> (e -> b) -> e -> c
dovekies a b c d e = a (b c) (d e)

bunting :: (a -> b) -> (c -> d -> e -> a) -> c -> d -> e -> b
bunting a b c d e = a (b c d e)
