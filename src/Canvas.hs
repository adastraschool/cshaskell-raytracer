module Canvas
    ( Canvas (..)
    , canvas
    , canvasWithColor
    , writePixel
    , pixelAt
    , ppm
    ) where

import Color (Color (..), black, ppmColor)
import Data.List (intercalate)

data Canvas = Canvas
    { width :: Int
    , height :: Int
    , pixels :: [[Color]]
    } deriving (Show)

canvasWithColor :: Int -> Int -> Color -> Canvas
canvasWithColor w h = Canvas w h
    . replicate h
    . replicate w

canvas :: Int -> Int -> Canvas
canvas w h = canvasWithColor w h black

updateList :: [a] -> Int -> a -> [a]
updateList list i =
    (take i list ++) . (++ drop (i+1) list) . return

updateLoL :: [[a]] -> Int -> Int -> a -> [[a]]
updateLoL listOfLists m n =
    updateList listOfLists m
        . updateList (listOfLists !! m) n

writePixel :: Canvas -> Int -> Int -> Color -> Canvas
writePixel c x y color =
    c { pixels = updateLoL (pixels c) y x color }

pixelAt :: Canvas -> Int -> Int -> Color
pixelAt c =
    flip ((!!) . (pixels c !!))

ppmHeader :: Canvas -> String
ppmHeader c =
    "P3\n" ++ show (width c) ++ " " ++ show (height c) ++ "\n255\n"

appendToRow :: ([String], Int) -> String -> ([String], Int)
appendToRow ([], _) s = ([s], length s)
appendToRow (row:rest, w) s
    | length s + w > 69 = (s:row:rest, length s)
    | otherwise = ((row ++ " " ++ s):rest, length s + w + 1)

ppmPixelsRow :: [Color] -> String
ppmPixelsRow = intercalate "\n"
    . reverse
    . fst
    . foldl appendToRow ([], 0)
    . concatMap ppmColor

ppmPixels :: [[Color]] -> String
ppmPixels =
    intercalate "\n" . map ppmPixelsRow

ppm :: Canvas -> String
ppm c = ppmHeader c
    ++ ppmPixels (pixels c)
    ++ "\n"
