module Main where

import Lib
import Matrix
import Canvas
import Color (Color, red, black, white)
import Sphere
import Intersection
import Ray

pointOnCanvas :: (RealFrac a, Floating a) => Tuple4 a -> Canvas -> Canvas
pointOnCanvas (Tuple4 x y _ _) c =
    writePixel c (round (fromIntegral (width c)/2 + x)) (round (fromIntegral (height c)/2 + y)) white

rotatePoint :: (RealFrac a, Floating a) => a -> Tuple4 a -> Tuple4 a
rotatePoint =
    multMatrixTuple4 . rotationZMatrix4

clock :: IO ()
clock = writeFile "clock.ppm"
    $ ppm
    $ foldr
        (pointOnCanvas . flip rotatePoint (point 0 75 0) . (*(pi/6)))
        (canvas 200 200)
        [0..11]

unitSphere :: Num a => Sphere a
unitSphere = Sphere identityMatrix4

spherePixel :: (Floating a, Ord a) => Tuple4 a -> a -> a -> a -> Color
spherePixel rayOrigin wallZ worldY worldX =
    maybe black (const red)
        $ hit
        $ intersect unitSphere
        $ Ray rayOrigin
        $ normTuple4
        $ point worldX worldY wallZ - rayOrigin

spherePixelsRow :: (Floating a, Ord a) => Int -> a -> Tuple4 a -> a -> a -> Int -> [Color]
spherePixelsRow canvasPixels pixelSize rayOrigin wallZ half y =
    map
        (spherePixel rayOrigin wallZ (half - pixelSize * fromIntegral y) . (-half +) . (pixelSize *) . fromIntegral)
        [0..canvasPixels-1]

castRayAtSphere :: IO ()
castRayAtSphere = writeFile "sphere.ppm"
    $ ppm
    $ let
        rayOrigin = point 0 0 (-5)
        wallZ = 10.0
        wallSize = 7.0
        canvasPixels = 100
        pixelSize = wallSize / fromIntegral canvasPixels
        half = wallSize / 2
    in
        Canvas canvasPixels canvasPixels
            $ map (spherePixelsRow canvasPixels pixelSize rayOrigin wallZ half) [0..canvasPixels-1]

main :: IO ()
main = castRayAtSphere
