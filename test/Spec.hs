module Main where

import Test.Hspec
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, forAll)
import Matrix (Matrix4 (..), Tuple4 (..), identityMatrix4, translationMatrix4, scalingMatrix4, rotationXMatrix4, rotationYMatrix4, rotationZMatrix4, shearingMatrix4, transposeMatrix4, point, vector, matrix2, matrix3, matrix4, matrix2Cell, matrix3Cell, matrix4Cell, isPoint, isVector, magTuple4, normTuple4, multTuple4, divTuple4, zipWithTuple4, zipWithMatrix4, dotTuple4, crossTuple4, multMatrixTuple4, determinant2, determinant3, determinant4, subMatrix3, subMatrix4, minor3, cofactor3, cofactor4, invertible, inverse)
import Comb (blackbird)
import Color (Color (..), zipWithColor, red, black, getRed, getGreen, getBlue, multScalarColor)
import Canvas (Canvas (..), canvas, canvasWithColor, writePixel, pixelAt, ppm)
import Ray (Ray (..), position, transform)
import Sphere (Sphere (..))
import Intersection (Intersection (..), intersect, hit)

simpleMatrix4 :: Matrix4 Float
simpleMatrix4 = Matrix4
    (Tuple4 1.2 2.3 3.4 4.5)
    (Tuple4 56.7 6 7.0 89.0)
    (Tuple4 923 0 1 23)
    (Tuple4 3 4 5 6)

epsilon :: (Floating a, Ord a) => a
epsilon = 0.00001

closeFloats :: (Floating a, Ord a) => a -> a -> Bool
closeFloats x = (< epsilon) . abs . (x -)

closeTuples :: (Floating a, Ord a) => Tuple4 a -> Tuple4 a -> Bool
closeTuples = blackbird and (zipWithTuple4 closeFloats)

closeMatrices :: (Floating a, Ord a) => Matrix4 a -> Matrix4 a -> Bool
closeMatrices = blackbird and (zipWithMatrix4 (zipWithTuple4 closeFloats))

closeColors :: Color -> Color -> Bool
closeColors (Color r1 g1 b1) (Color r2 g2 b2) =
    closeFloats r1 r2
    && closeFloats g1 g2
    && closeFloats b1 b2

instance (Arbitrary a) => Arbitrary (Matrix4 a) where
    arbitrary = do
        m00 <- arbitrary
        m01 <- arbitrary
        m02 <- arbitrary
        m03 <- arbitrary
        m10 <- arbitrary
        m11 <- arbitrary
        m12 <- arbitrary
        m13 <- arbitrary
        m20 <- arbitrary
        m21 <- arbitrary
        m22 <- arbitrary
        m23 <- arbitrary
        m30 <- arbitrary
        m31 <- arbitrary
        m32 <- arbitrary
        m33 <- arbitrary
        return (Matrix4
            (Tuple4 m00 m01 m02 m03)
            (Tuple4 m10 m11 m12 m13)
            (Tuple4 m20 m21 m22 m23)
            (Tuple4 m30 m31 m32 m33))

propertySimpleMatrix :: Property
propertySimpleMatrix =
    forAll (arbitrary :: Gen (Matrix4 Float)) (const True)

main :: IO ()
main = hspec $ do
    describe "Tuple" $ do
        it "is a point if w is 1.0" $ do
            Tuple4 4.3 (-4.2) 3.1 1.0 `shouldSatisfy` isPoint
            Tuple4 4.3 (-4.2) 3.1 1.0 `shouldNotSatisfy` isVector
        it "is a vector if w is 0.0" $ do
            Tuple4 4.3 (-4.2) 3.1 0.0 `shouldSatisfy` isVector
            Tuple4 4.3 (-4.2) 3.1 0.0 `shouldNotSatisfy` isPoint
        it "has w as 1 if created as point" $ --do
            point 4 (-4) 3 `shouldBe` Tuple4 4 (-4) 3 1
        it "has w as 0 if created as vector" $ --do
            vector 4 (-4) 3 `shouldBe` Tuple4 4 (-4) 3 0
        it "adds" $
            Tuple4 3 (-2) 5 1 + Tuple4 (-2) 3 1 0 `shouldBe` Tuple4 1 1 6 1
        it "points are subtracted to get a vector" $
            point 3 2 1 - point 5 6 7 `shouldBe` vector (-2) (-4) (-6)
        it "vectors subtracted from a point is a point" $
            point 3 2 1 - vector 5 6 7 `shouldBe` point (-2) (-4) (-6)
        it "vectors can be added" $
            vector 3 2 1 - vector 5 6 7 `shouldBe` vector (-2) (-4) (-6)
        it "vectors can be subtracted from the zero vector" $
            vector 0 0 0 - vector 1 (-2) 3 `shouldBe` vector (-1) 2 (-3)
        it "negates" $
            negate (Tuple4 1 (-2) 3 (-4)) `shouldBe` Tuple4 (-1) 2 (-3) 4
        it "multiplies by a scalar" $ do
            multTuple4 3.5 (Tuple4 1 (-2) 3 (-4)) `shouldBe` Tuple4 3.5 (-7) 10.5 (-14)
            multTuple4 0.5 (Tuple4 1 (-2) 3 (-4)) `shouldBe` Tuple4 0.5 (-1) 1.5 (-2)
        it "divides by a scalar" $
            divTuple4 (Tuple4 1 (-2) 3 (-4)) 2 `shouldBe` Tuple4 0.5 (-1) 1.5 (-2)
        it "has magnitude" $ do
            magTuple4 (vector 1 0 0) `shouldBe` 1
            magTuple4 (vector 0 1 0) `shouldBe` 1
            magTuple4 (vector 0 0 1) `shouldBe` 1
            magTuple4 (vector 1 2 3) `shouldBe` sqrt 14
            magTuple4 (vector (-1) (-2) (-3)) `shouldBe` sqrt 14
        it "normalizes" $ do
            normTuple4 (vector 4 0 0) `shouldBe` vector 1 0 0
            normTuple4 (vector 1 2 3) `shouldSatisfy` closeTuples (vector 0.26726 0.53452 0.80178)
            magTuple4 (normTuple4 (vector 1 2 3)) `shouldBe` 1
        it "has dot products" $
            dotTuple4 (vector 1 2 3) (vector 2 3 4) `shouldBe` 20
        it "has cross products" $ do
            crossTuple4 (vector 1 2 3) (vector 2 3 4) `shouldBe` vector (-1) 2 (-1)
            crossTuple4 (vector 2 3 4) (vector 1 2 3) `shouldBe` vector 1 (-2) 1
    describe "Color" $ do
        it "has red, green, and blue components" $ do
            Color (-0.5) 0.4 1.7 `shouldSatisfy` (==(-0.5)) . getRed
            Color (-0.5) 0.4 1.7 `shouldSatisfy` (==0.4) . getGreen
            Color (-0.5) 0.4 1.7 `shouldSatisfy` (==1.7) . getBlue
        it "adds" $
            Color 0.9 0.6 0.75 + Color 0.7 0.1 0.25 `shouldSatisfy` closeColors (Color 1.6 0.7 1.0)
        it "subtracts" $
            Color 0.9 0.6 0.75 - Color 0.7 0.1 0.25 `shouldSatisfy` closeColors (Color 0.2 0.5 0.5)
        it "multiplies by a scalar" $
            multScalarColor 2 (Color 0.2 0.3 0.4) `shouldBe` Color 0.4 0.6 0.8
        it "multiplies" $
            Color 1 0.2 0.4 * Color 0.9 1 0.1 `shouldSatisfy` closeColors (Color 0.9 0.2 0.04)
    describe "Canvas" $ do
        it "gets created" $
            let
                c = canvas 10 20
            in
                do
                    width c `shouldBe` 10
                    height c `shouldBe` 20
                    pixels c `shouldSatisfy` all (==black) . concat
        it "writes pixels" $
            pixelAt (writePixel (canvas 10 20) 2 3 red) 2 3
                `shouldBe` red
        it "constructs PPM pixel data" $
            let
                c = canvas 5 3
                c1 = writePixel c 0 0 (Color 1.5 0 0)
                c2 = writePixel c1 2 1 (Color 0 0.5 0)
                c3 = writePixel c2 4 2 (Color (-0.5) 0 1)
            in
                ppm c3 `shouldBe` "P3\n5 3\n255\n255 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 128 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0 0 0 0 0 0 255\n"
        it "splits long lines in PPM files" $
            ppm (canvasWithColor 10 2 (Color 1 0.8 0.6))
                `shouldBe` "P3\n10 2\n255\n255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n153 255 204 153 255 204 153 255 204 153 255 204 153\n255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n153 255 204 153 255 204 153 255 204 153 255 204 153\n"
    describe "Matrix" $ do
        it "constructs 4x4 matrices" $
            let
                m = matrix4 1 2 3 4 5.5 6.5 7.5 8.5 9 10 11 12 13.5 14.5 15.5 16.5
            in
                do
                    matrix4Cell m 0 0 `shouldBe` 1
                    matrix4Cell m 0 3 `shouldBe` 4
                    matrix4Cell m 1 0 `shouldBe` 5.5
                    matrix4Cell m 1 2 `shouldBe` 7.5
                    matrix4Cell m 2 2 `shouldBe` 11
                    matrix4Cell m 3 0 `shouldBe` 13.5
                    matrix4Cell m 3 2 `shouldBe` 15.5
        it "constructs 3x3 matrices" $
            let
                m = matrix3 (-3) 5 0 1 (-2) (-7) 0 1 1
            in
                do
                    matrix3Cell m 0 0 `shouldBe` (-3)
                    matrix3Cell m 1 1 `shouldBe` (-2)
                    matrix3Cell m 2 2 `shouldBe` 1
        it "constructs 2x2 matrices" $
            let
                m = matrix2 (-3) 5 1 (-2)
            in
                do
                    matrix2Cell m 0 0 `shouldBe` (-3)
                    matrix2Cell m 0 1 `shouldBe` 5
                    matrix2Cell m 1 0 `shouldBe` 1
                    matrix2Cell m 1 1 `shouldBe` (-2)
        it "should be equal to an identical matrix" $ do
            matrix4 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2
                `shouldBe` matrix4 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2
            matrix4 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2
                `shouldNotBe` matrix4 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2 1
        it "multiplies" $
            matrix4
                1 2 3 4
                5 6 7 8
                9 8 7 6
                5 4 3 2
                <> matrix4
                    (-2) 1 2 3
                    3 2 1 (-1)
                    4 3 6 5
                    1 2 7 8
                    `shouldBe` matrix4
                        20 22 50 48
                        44 54 114 108
                        40 58 110 102
                        16 26 46 42
        it "multiplies by a tuple" $
            multMatrixTuple4
                (matrix4 1 2 3 4 2 4 4 2 8 6 4 1 0 0 0 1)
                (Tuple4 1 2 3 1)
                `shouldBe` Tuple4 18 24 33 1
        it "multiply by the identity matrix to get itself" $
            matrix4 0 1 2 4 1 2 4 8 2 4 8 16 4 8 16 32
                <> identityMatrix4
                    `shouldBe` matrix4 0 1 2 4 1 2 4 8 2 4 8 16 4 8 16 32
        it "transposes" $
            transposeMatrix4 (matrix4 0 9 3 0 9 8 0 8 1 8 5 3 0 0 5 8)
                `shouldBe` matrix4 0 9 1 0 9 8 8 0 3 0 5 5 0 8 3 8
        it "transposes the identity matrix" $
            transposeMatrix4 identityMatrix4 `shouldBe` identityMatrix4
        it "has 2x2 determinants" $
            determinant2 (matrix2 1 5 (-3) 2) `shouldBe` 17
        it "has 2x2 submatrices" $
            subMatrix3 (matrix3 1 5 0 (-3) 2 7 0 6 (-3)) 0 2
                `shouldBe` matrix2 (-3) 2 0 6
        it "has 3x3 submatrices" $
            subMatrix4 (matrix4 (-6) 1 1 6 (-8) 5 8 6 (-1) 0 8 2 (-7) 1 (-1) 1) 2 1
                `shouldBe` matrix3 (-6) 1 6 (-8) 8 6 (-7) (-1) 1
        it "has a 3x3 minor" $
            let
                a = matrix3 3 5 0 2 (-1) (-7) 6 (-1) 5
            in
                do
                    determinant2 (subMatrix3 a 1 0) `shouldBe` 25
                    minor3 a 1 0 `shouldBe` 25
        it "has 3x3 cofactors" $
            let
                a = matrix3 3 5 0 2 (-1) (-7) 6 (-1) 5
            in
                do
                    minor3 a 0 0 `shouldBe` (-12)
                    cofactor3 a 0 0 `shouldBe` (-12)
                    minor3 a 1 0 `shouldBe` 25
                    cofactor3 a 1 0 `shouldBe` (-25)
        it "has 3x3 determinants" $
            let
                a = matrix3 1 2 6 (-5) 8 (-4) 2 6 4
            in
                do
                    cofactor3 a 0 0 `shouldBe` 56
                    cofactor3 a 0 1 `shouldBe` 12
                    cofactor3 a 0 2 `shouldBe` (-46)
                    determinant3 a `shouldBe` (-196)
        it "has 4x4 determinants" $
            let
                a = matrix4 (-2) (-8) 3 5 (-3) 1 7 3 1 2 (-9) 6 (-6) 7 7 (-9)
            in
                do
                    cofactor4 a 0 0 `shouldBe` 690
                    cofactor4 a 0 1 `shouldBe` 447
                    cofactor4 a 0 2 `shouldBe` 210
                    cofactor4 a 0 3 `shouldBe` 51
                    determinant4 a `shouldBe` (-4071)
        it "can be invertible or non-invertible" $
            let
                a = matrix4 6 4 4 4 5 5 7 6 4 (-9) 3 (-7) 9 1 7 (-6)
                b = matrix4 (-4) 2 (-2) (-3) 9 6 2 6 0 (-5) 1 (-5) 0 0 0 0
            in
                do
                    determinant4 a `shouldBe` (-2120)
                    a `shouldSatisfy` invertible
                    determinant4 b `shouldBe` 0
                    b `shouldNotSatisfy` invertible
        it "might have an inverse" $
            let
                a = matrix4 (-5) 2 6 (-8) 1 (-5) 1 8 7 7 (-6) (-7) 1 (-3) 7 4
                b = inverse a
                c = matrix4 8 (-5) 9 2 7 5 6 1 (-6) 0 9 6 (-3) 0 (-9) (-4)
                d = matrix4 9 3 0 9 (-5) (-2) (-6) (-3) (-4) 9 6 4 (-7) 6 6 2
            in
                do
                    determinant4 a `shouldBe` 532
                    cofactor4 a 2 3 `shouldBe` (-160)
                    matrix4Cell b 3 2 `shouldBe` (-160)/532
                    cofactor4 a 3 2 `shouldBe` 105
                    matrix4Cell b 2 3 `shouldBe` 105/532
                    b `shouldSatisfy` closeMatrices (matrix4 0.21805 0.45113 0.24060 (-0.04511) (-0.80827) (-1.45677) (-0.44361) 0.52068 (-0.07895) (-0.22368) (-0.05263) 0.19737 (-0.52256) (-0.81391) (-0.30075) 0.30639)
                    inverse c `shouldSatisfy` closeMatrices (matrix4 (-0.15385) (-0.15385) (-0.28205) (-0.53846) (-0.07692) 0.12308 0.02564 0.03077 0.35897 0.35897 0.43590 0.92308 (-0.69231) (-0.69231) (-0.76923) (-1.92308))
                    inverse d `shouldSatisfy` closeMatrices (matrix4 (-0.04074) (-0.07778) 0.14444 (-0.22222) (-0.07778) 0.03333 0.36667 (-0.33333) (-0.02901) (-0.14630) (-0.10926) 0.12963 0.17778 0.06667 (-0.26667) 0.33333)
        it "multiplies by a matrix and its inverse" $
            let
                a = matrix4 3 (-9) 7 3 3 (-8) 2 (-9) (-4) 4 4 1 (-6) 5 (-1) 1
                b = matrix4 8 2 2 2 3 (-1) 7 0 7 0 5 4 6 (-2) 0 5
            in
                a <> b <> inverse b `shouldSatisfy` closeMatrices a
    describe "Transformations" $ do
        it "multiply a translation matrix" $
            multMatrixTuple4 (translationMatrix4 5 (-3) 2) (point (-3) 4 5)
                `shouldBe` point 2 1 7
        it "multiply by the inverse of a translation matrix" $
            multMatrixTuple4 (inverse (translationMatrix4 5 (-3) 2)) (point (-3) 4 5)
                `shouldBe` point (-8) 7 3
        it "translation does not affect vectors" $
            multMatrixTuple4 (translationMatrix4 5 (-3) 2) (vector (-3) 4 5)
                `shouldBe` vector (-3) 4 5
        it "apply a scaling matrix to a point" $
            multMatrixTuple4 (scalingMatrix4 2 3 4) (point (-4) 6 8)
                `shouldBe` point (-8) 18 32
        it "apply a scaling matrix to a vector" $
            multMatrixTuple4 (scalingMatrix4 2 3 4) (vector (-4) 6 8)
                `shouldBe` vector (-8) 18 32
        it "multiply by the inverse of a scaling matrix" $
            multMatrixTuple4 (inverse (scalingMatrix4 2 3 4)) (vector (-4) 6 8)
                `shouldBe` vector (-2) 2 2
        it "reflect by scaling a negative value" $
            multMatrixTuple4 (scalingMatrix4 (-1) 1 1) (point 2 3 4)
                `shouldBe` point (-2) 3 4
        it "rotate a point around the x axis" $ do
            multMatrixTuple4 (rotationXMatrix4 (pi/4)) (point 0 1 0)
                `shouldSatisfy` closeTuples (point 0 (sqrt 2 / 2) (sqrt 2 / 2))
            multMatrixTuple4 (rotationXMatrix4 (pi/2)) (point 0 1 0)
                `shouldSatisfy` closeTuples (point 0 0 1)
        it "rotate in the opposite direction with an inverse matrix" $
            multMatrixTuple4 (inverse (rotationXMatrix4 (pi/4))) (point 0 1 0)
                `shouldSatisfy` closeTuples (point 0 (sqrt 2 / 2) (-sqrt 2 / 2))
        it "rotate a point around the y axis" $ do
            multMatrixTuple4 (rotationYMatrix4 (pi/4)) (point 0 0 1)
                `shouldSatisfy` closeTuples (point (sqrt 2 / 2) 0 (sqrt 2 / 2))
            multMatrixTuple4 (rotationYMatrix4 (pi/2)) (point 0 0 1)
                `shouldSatisfy` closeTuples (point 1 0 0)
        it "rotate a point around the z axis" $ do
            multMatrixTuple4 (rotationZMatrix4 (pi/4)) (point 0 1 0)
                `shouldSatisfy` closeTuples (point (-sqrt 2 / 2) (sqrt 2 / 2) 0)
            multMatrixTuple4 (rotationZMatrix4 (pi/2)) (point 0 1 0)
                `shouldSatisfy` closeTuples (point (-1) 0 0)
        it "shear points" $ do
            multMatrixTuple4 (shearingMatrix4 0 1 0 0 0 0) (point 2 3 4)
                `shouldBe` point 6 3 4
            multMatrixTuple4 (shearingMatrix4 0 0 1 0 0 0) (point 2 3 4)
                `shouldBe` point 2 5 4
            multMatrixTuple4 (shearingMatrix4 0 0 0 1 0 0) (point 2 3 4)
                `shouldBe` point 2 7 4
            multMatrixTuple4 (shearingMatrix4 0 0 0 0 1 0) (point 2 3 4)
                `shouldBe` point 2 3 6
            multMatrixTuple4 (shearingMatrix4 0 0 0 0 0 1) (point 2 3 4)
                `shouldBe` point 2 3 7
        it "chain in reverse order" $
            multMatrixTuple4 (translationMatrix4 10 5 7)
                (multMatrixTuple4 (scalingMatrix4 5 5 5)
                    (multMatrixTuple4 (rotationXMatrix4 (pi/2)) (point 1 0 1)))
                `shouldBe` multMatrixTuple4
                    (translationMatrix4 10 5 7 <> scalingMatrix4 5 5 5 <> rotationXMatrix4 (pi/2))
                    (point 1 0 1)
    describe "Rays" $ do
        it "compute a point from a distance" $
            let
                r = Ray (point 2 3 4) (vector 1 0 0)
            in
                do
                    position r 0 `shouldBe` point 2 3 4
                    position r 1 `shouldBe` point 3 3 4
                    position r (-1) `shouldBe` point 1 3 4
                    position r 2.5 `shouldBe` point 4.5 3 4
        it "intersect with a sphere at 2 points" $
            let
                s = Sphere identityMatrix4
                xs :: [Intersection Float]
                xs = intersect s (Ray (point 0 0 (-5)) (vector 0 0 1))
            in
                do
                    length xs `shouldBe` 2
                    xs `shouldBe` [Intersection 4.0 s, Intersection 6.0 s]
        it "intersect with a sphere at a tangent" $
            let
                s = Sphere identityMatrix4
                xs :: [Intersection Float]
                xs = intersect s (Ray (point 0 1 (-5)) (vector 0 0 1))
            in
                do
                    length xs `shouldBe` 2
                    xs `shouldBe` [Intersection 5.0 s, Intersection 5.0 s]
        it "miss a sphere" $
            length (Sphere identityMatrix4 `intersect` Ray (point 0 2 (-5)) (vector 0 0 1))
                `shouldBe` 0
        it "originate inside a sphere" $
            let
                s = Sphere identityMatrix4
                xs :: [Intersection Float]
                xs = intersect s (Ray (point 0 0 0) (vector 0 0 1))
            in
                do
                    length xs `shouldBe` 2
                    xs `shouldBe` [Intersection (-1.0) s, Intersection 1.0 s]
        it "are in front of a sphere" $
            let
                s = Sphere identityMatrix4
                xs :: [Intersection Float]
                xs = intersect s (Ray (point 0 0 5) (vector 0 0 1))
            in
                do
                    length xs `shouldBe` 2
                    xs `shouldBe` [Intersection (-6.0) s, Intersection (-4.0) s]
        it "intersect with all positive t" $
            let
                s = Sphere identityMatrix4
                i1 = Intersection 1 s
                i2 = Intersection 2 s
            in
                hit [i1, i2] `shouldBe` Just i1
        it "intersect with a negative t" $
            let
                s = Sphere identityMatrix4
                i1 = Intersection (-1) s
                i2 = Intersection 1 s
            in
                hit [i1, i2] `shouldBe` Just i2
        it "intersect with all negative t" $
            let
                s = Sphere identityMatrix4
                i1 = Intersection (-2) s
                i2 = Intersection (-1) s
            in
                hit [i1, i2] `shouldBe` Nothing
        it "hit with the lowest non-negative intersection" $
            let
                s = Sphere identityMatrix4
                i1 = Intersection 5 s
                i2 = Intersection 7 s
                i3 = Intersection (-3) s
                i4 = Intersection 2 s
            in
                hit [i1, i2, i3, i4] `shouldBe` Just i4
        it "translate" $
            transform
                (translationMatrix4 3 4 5)
                (Ray (point 1 2 3) (vector 0 1 0))
                `shouldBe` Ray (point 4 6 8) (vector 0 1 0)
        it "scale" $
            transform
                (scalingMatrix4 2 3 4)
                (Ray (point 1 2 3) (vector 0 1 0))
                `shouldBe` Ray (point 2 6 12) (vector 0 3 0)
        it "intersects a scaled sphere" $
            let
                s = Sphere (scalingMatrix4 2 2 2)
                r = Ray (point 0 0 (-5)) (vector 0 0 1)
            in
                intersect s r `shouldBe`
                    [ Intersection 3 s, Intersection 7 s ]
        it "intersects a translated sphere" $
            let
                s = Sphere (translationMatrix4 5 0 0)
                r = Ray (point 0 0 (-5)) (vector 0 0 1)
            in
                intersect s r `shouldBe` []
