module Color
    ( Color (..)
    , zipWithColor
    , red, black, white
    , getRed, getGreen, getBlue
    , multScalarColor
    , ppmColor
    ) where

data Color = Color Float Float Float deriving (Show, Eq)

cmap :: (Float -> Float) -> Color -> Color
cmap f (Color r g b) = Color (f r) (f g) (f b)

cmapList :: (Float -> a) -> Color -> [a]
cmapList f (Color r g b) = [f r, f g, f b]

colorRepeat :: Float -> Color
colorRepeat x = Color x x x

zipWithColor :: (Float -> Float -> Float) -> Color -> Color -> Color
zipWithColor f (Color r1 g1 b1) (Color r2 g2 b2) =
    Color (f r1 r2) (f g1 g2) (f b1 b2)

instance Num Color where
    (+) = zipWithColor (+)
    (-) = zipWithColor (-)
    (*) = zipWithColor (*)
    negate = cmap negate
    abs = cmap abs
    signum = cmap signum
    fromInteger = colorRepeat . fromInteger

red :: Color
red = Color 1 0 0

black :: Color
black = Color 0 0 0

white :: Color
white = Color 1 1 1

getRed :: Color -> Float
getRed (Color r _ _) = r

getGreen :: Color -> Float
getGreen (Color _ g _) = g

getBlue :: Color -> Float
getBlue (Color _ _ b) = b

multScalarColor :: Float -> Color -> Color
multScalarColor = cmap . (*)

expandAndClamp :: Float -> Int
expandAndClamp = min 255 . max 0 . floor . (*256)

ppmColor :: Color -> [String]
ppmColor = map show . cmapList expandAndClamp
