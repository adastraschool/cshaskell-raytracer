module Matrix
    ( Tuple2 (..)
    , Tuple3 (..)
    , Tuple4 (..)
    , Matrix2 (..)
    , Matrix3 (..)
    , Matrix4 (..)
    , identityMatrix4, translationMatrix4, scalingMatrix4
    , rotationXMatrix4, rotationYMatrix4, rotationZMatrix4
    , shearingMatrix4
    , point
    , vector
    , matrix2, matrix3, matrix4
    , matrix2Cell, matrix3Cell, matrix4Cell
    , isPoint
    , isVector
    , transposeMatrix4
    , magTuple4
    , normTuple4
    , multTuple4
    , divTuple4
    , zipWithTuple4, zipWithMatrix4
    , dotTuple4
    , crossTuple4
    , multMatrixTuple4
    , determinant2, determinant3, determinant4
    , subMatrix3, subMatrix4
    , minor3, cofactor3, minor4, cofactor4
    , invertible
    , inverse
    ) where

import Comb (starling, goldfinch, blackbird, dovekies, bunting)
import Data.List (intercalate, zipWith4)

data Tuple2 a = Tuple2 a a deriving (Eq, Show)
data Tuple3 a = Tuple3 a a a deriving (Eq, Show)
data Tuple4 a = Tuple4 a a a a deriving (Eq)
data Matrix2 a = Matrix2 (Tuple2 a) (Tuple2 a) deriving (Eq, Show)
data Matrix3 a = Matrix3 (Tuple3 a) (Tuple3 a) (Tuple3 a) deriving (Eq, Show)
data Matrix4 a = Matrix4 (Tuple4 a) (Tuple4 a) (Tuple4 a) (Tuple4 a) deriving (Eq)

identityMatrix4 :: Num a => Matrix4 a
identityMatrix4 = Matrix4 (Tuple4 1 0 0 0) (Tuple4 0 1 0 0) (Tuple4 0 0 1 0) (Tuple4 0 0 0 1)

translationMatrix4 :: Num a => a -> a -> a -> Matrix4 a
translationMatrix4 x y z =
    matrix4
        1 0 0 x
        0 1 0 y
        0 0 1 z
        0 0 0 1

scalingMatrix4 :: Num a => a -> a -> a -> Matrix4 a
scalingMatrix4 x y z =
    matrix4
        x 0 0 0
        0 y 0 0
        0 0 z 0
        0 0 0 1

rotationXMatrix4 :: Floating a => a -> Matrix4 a
rotationXMatrix4 x =
  matrix4
    1 0 0 0
    0 (cos x) (-sin x) 0
    0 (sin x) (cos x) 0
    0 0 0 1

rotationYMatrix4 :: Floating a => a -> Matrix4 a
rotationYMatrix4 x =
  matrix4
    (cos x) 0 (sin x) 0
    0 1 0 0
    (-sin x) 0 (cos x) 0
    0 0 0 1

rotationZMatrix4 :: Floating a => a -> Matrix4 a
rotationZMatrix4 x =
  matrix4
    (cos x) (-sin x) 0 0
    (sin x) (cos x) 0 0
    0 0 1 0
    0 0 0 1

shearingMatrix4 :: Num a => a -> a -> a -> a -> a -> a -> Matrix4 a
shearingMatrix4 xy xz yx yz zx zy =
  matrix4
    1 xy xz 0
    yx 1 yz 0
    zx zy 1 0
    0 0 0 1

point :: Num a => a -> a -> a -> Tuple4 a
point x y z = Tuple4 x y z 1

vector :: Num a => a -> a -> a -> Tuple4 a
vector x y z = Tuple4 x y z 0

matrix2 :: a -> a -> a -> a -> Matrix2 a
matrix2 c00 c01 c10 c11 =
    Matrix2
        (Tuple2 c00 c01)
        (Tuple2 c10 c11)

matrix3 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix3 a
matrix3 c00 c01 c02 c10 c11 c12 c20 c21 c22 =
    Matrix3
        (Tuple3 c00 c01 c02)
        (Tuple3 c10 c11 c12)
        (Tuple3 c20 c21 c22)

matrix4 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix4 a
matrix4 c00 c01 c02 c03 c10 c11 c12 c13 c20 c21 c22 c23 c30 c31 c32 c33 =
    Matrix4
        (Tuple4 c00 c01 c02 c03)
        (Tuple4 c10 c11 c12 c13)
        (Tuple4 c20 c21 c22 c23)
        (Tuple4 c30 c31 c32 c33)

matrix4FromList :: [a] -> Matrix4 a
matrix4FromList [c00, c01, c02, c03, c10, c11, c12, c13, c20, c21, c22, c23, c30, c31, c32, c33] =
    matrix4 c00 c01 c02 c03 c10 c11 c12 c13 c20 c21 c22 c23 c30 c31 c32 c33

tuple2Cell :: Tuple2 a -> Int -> a
tuple2Cell (Tuple2 cell _) 0 = cell
tuple2Cell (Tuple2 _ cell) 1 = cell

matrix2Row :: Matrix2  a -> Int -> Tuple2 a
matrix2Row (Matrix2 row _) 0 = row
matrix2Row (Matrix2 _ row) 1 = row

matrix2Cell :: Matrix2 a -> Int -> Int -> a
matrix2Cell = blackbird tuple2Cell matrix2Row

tuple3Cell :: Tuple3 a -> Int -> a
tuple3Cell (Tuple3 cell _ _) 0 = cell
tuple3Cell (Tuple3 _ cell _) 1 = cell
tuple3Cell (Tuple3 _ _ cell) 2 = cell

matrix3Row :: Matrix3  a -> Int -> Tuple3 a
matrix3Row (Matrix3 row _ _) 0 = row
matrix3Row (Matrix3 _ row _) 1 = row
matrix3Row (Matrix3 _ _ row) 2 = row

matrix3Cell :: Matrix3 a -> Int -> Int -> a
matrix3Cell = blackbird tuple3Cell matrix3Row

tuple4Cell :: Tuple4 a -> Int -> a
tuple4Cell (Tuple4 cell _ _ _) 0 = cell
tuple4Cell (Tuple4 _ cell _ _) 1 = cell
tuple4Cell (Tuple4 _ _ cell _) 2 = cell
tuple4Cell (Tuple4 _ _ _ cell) 3 = cell

matrix4Row :: Matrix4 a -> Int -> Tuple4 a
matrix4Row (Matrix4 row _ _ _) 0 = row
matrix4Row (Matrix4 _ row _ _) 1 = row
matrix4Row (Matrix4 _ _ row _) 2 = row
matrix4Row (Matrix4 _ _ _ row) 3 = row

matrix4Cell :: Matrix4 a -> Int -> Int -> a
matrix4Cell = blackbird tuple4Cell matrix4Row

isPoint :: Tuple4 Float -> Bool
isPoint (Tuple4 _ _ _ w) = w == 1.0

isVector :: Tuple4 Float -> Bool
isVector (Tuple4 _ _ _ w) = w == 0.0

tuple4Repeat :: a -> Tuple4 a
tuple4Repeat a = Tuple4 a a a a

instance Num a => Num (Tuple4 a) where
    (+) = zipWithTuple4 (+)
    (-) = zipWithTuple4 (-)
    (*) = zipWithTuple4 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = tuple4Repeat . fromInteger

instance Foldable Tuple4 where
    foldr f z = foldr f z . listFromTuple4

instance Functor Tuple4 where
    fmap f (Tuple4 a b c d) = Tuple4 (f a) (f b) (f c) (f d)

instance Functor Matrix4 where
    fmap f (Matrix4
        (Tuple4 x1 x2 x3 x4)
        (Tuple4 y1 y2 y3 y4)
        (Tuple4 z1 z2 z3 z4)
        (Tuple4 w1 w2 w3 w4)) =
            Matrix4
                (Tuple4 (f x1) (f x2) (f x3) (f x4))
                (Tuple4 (f y1) (f y2) (f y3) (f y4))
                (Tuple4 (f z1) (f z2) (f z3) (f z4))
                (Tuple4 (f w1) (f w2) (f w3) (f w4))

instance Foldable Matrix4 where
    foldr f z = foldr f z . listFromMatrix4

transposeMatrix4 :: Matrix4 a -> Matrix4 a
transposeMatrix4 (Matrix4
    (Tuple4 c00 c01 c02 c03)
    (Tuple4 c10 c11 c12 c13)
    (Tuple4 c20 c21 c22 c23)
    (Tuple4 c30 c31 c32 c33)) =
        matrix4
            c00 c10 c20 c30
            c01 c11 c21 c31
            c02 c12 c22 c32
            c03 c13 c23 c33

magTuple4 :: Floating a => Tuple4 a -> a
magTuple4 = sqrt . sum . fmap (^2)

normTuple4 :: Floating a => Tuple4 a -> Tuple4 a
normTuple4 = starling divTuple4 magTuple4

dotTuple4 :: Num a => Tuple4 a -> Tuple4 a -> a
dotTuple4 tuple =
    sum . (tuple *)

crossTuple4 :: Num a => Tuple4 a -> Tuple4 a -> Tuple4 a
crossTuple4 (Tuple4 x1 y1 z1 w1) (Tuple4 x2 y2 z2 w2) =
    vector (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

dotMatrix4 :: Num a => Matrix4 a -> Matrix4 a -> Matrix4 a
dotMatrix4 (Matrix4 r0 r1 r2 r3) (Matrix4 c0 c1 c2 c3) =
    matrix4
        (dotTuple4 r0 c0) (dotTuple4 r0 c1) (dotTuple4 r0 c2) (dotTuple4 r0 c3)
        (dotTuple4 r1 c0) (dotTuple4 r1 c1) (dotTuple4 r1 c2) (dotTuple4 r1 c3)
        (dotTuple4 r2 c0) (dotTuple4 r2 c1) (dotTuple4 r2 c2) (dotTuple4 r2 c3)
        (dotTuple4 r3 c0) (dotTuple4 r3 c1) (dotTuple4 r3 c2) (dotTuple4 r3 c3)

instance Num a => Semigroup (Matrix4 a) where
    (<>) = flip $ goldfinch dotMatrix4 transposeMatrix4

listFromTuple4 :: Tuple4 a -> [a]
listFromTuple4 (Tuple4 x y z w) = [x, y, z, w]

listFromMatrix4 :: Matrix4 a -> [a]
listFromMatrix4 (Matrix4
    (Tuple4 x1 x2 x3 x4)
    (Tuple4 y1 y2 y3 y4)
    (Tuple4 z1 z2 z3 z4)
    (Tuple4 w1 w2 w3 w4)) =
        [ x1, y1, z1, w1
        , x2, y2, z2, w2
        , x3, y3, z3, w3
        , x4, y4, z4, w4
        ]

tuplesFromMatrix4 :: Matrix4 a -> Tuple4 (Tuple4 a)
tuplesFromMatrix4 (Matrix4 a b c d) = Tuple4 a b c d

tuplesListFromMatrix4 :: Matrix4 a -> [Tuple4 a]
tuplesListFromMatrix4 (Matrix4 a b c d) = [a, b, c, d]

zipWithTuple4 :: (a -> b -> c) -> Tuple4 a -> Tuple4 b -> Tuple4 c
zipWithTuple4 f (Tuple4 x1 y1 z1 w1) (Tuple4 x2 y2 z2 w2) =
    Tuple4 (f x1 x2) (f y1 y2) (f z1 z2) (f w1 w2)

zipWithMatrix4 :: (Tuple4 a -> Tuple4 b -> Tuple4 c) -> Matrix4 a -> Matrix4 b -> Matrix4 c
zipWithMatrix4 f (Matrix4 x1 y1 z1 w1) (Matrix4 x2 y2 z2 w2) =
    Matrix4 (f x1 x2) (f y1 y2) (f z1 z2) (f w1 w2)

bestLength :: Show a => a -> Int -> Int
bestLength a b =
    max b (length (show a))

maxSizesTuple4 :: Show a => Tuple4 a -> Tuple4 Int -> Tuple4 Int
maxSizesTuple4 = zipWithTuple4 bestLength

maxSizesMatrix4 :: Show a => Matrix4 a -> Tuple4 Int
maxSizesMatrix4 = foldr maxSizesTuple4 (Tuple4 0 0 0 0) . tuplesListFromMatrix4

pad :: Show a => Int -> a -> String
pad width val =
    replicate padding ' ' ++ string
        where
            string = show val
            chars = length string
            padding = max 0 $ width - chars

showWithPadding :: Show a => Tuple4 Int -> Tuple4 a -> String
showWithPadding tupleSpaces
    = unwords
    . listFromTuple4
    . zipWithTuple4 pad tupleSpaces

instance (Show a) => Show (Tuple4 a) where
    show (Tuple4 x y z w) = "[ " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " " ++ show w ++ " ]"

bracketize :: String -> String
bracketize =
    ("[ " ++) . (++ " ]")

tuple4Strings :: Show a => Matrix4 a -> [Tuple4 a] -> [String]
tuple4Strings =
    map . showWithPadding . maxSizesMatrix4

instance (Show a) => Show (Matrix4 a) where
    show = intercalate "\n" . map bracketize . starling tuple4Strings tuplesListFromMatrix4

multTuple4 :: Num a => a -> Tuple4 a -> Tuple4 a
multTuple4 = fmap . (*)

divTuple4 :: Fractional a => Tuple4 a -> a -> Tuple4 a
divTuple4 = flip (fmap . flip (/))

multMatrixTuple4 :: Num a => Matrix4 a -> Tuple4 a -> Tuple4 a
multMatrixTuple4 = flip $ flip (dovekies fmap dotTuple4) tuplesFromMatrix4

determinant2 :: Num a => Matrix2 a -> a
determinant2 (Matrix2 (Tuple2 a b) (Tuple2 c d)) = a * d - b * c

subMatrix3FromTuple3s :: Tuple3 a -> Tuple3 a -> Int -> Matrix2 a
subMatrix3FromTuple3s (Tuple3 _ x1 y1) (Tuple3 _ x2 y2) 0 = matrix2 x1 y1 x2 y2
subMatrix3FromTuple3s (Tuple3 x1 _ y1) (Tuple3 x2 _ y2) 1 = matrix2 x1 y1 x2 y2
subMatrix3FromTuple3s (Tuple3 x1 y1 _) (Tuple3 x2 y2 _) 2 = matrix2 x1 y1 x2 y2

subMatrix3 :: Matrix3 a -> Int -> Int -> Matrix2 a
subMatrix3 (Matrix3 _ a b) 0 n = subMatrix3FromTuple3s a b n
subMatrix3 (Matrix3 a _ b) 1 n = subMatrix3FromTuple3s a b n
subMatrix3 (Matrix3 a b _) 2 n = subMatrix3FromTuple3s a b n

subMatrix4FromTuple4s :: Tuple4 a -> Tuple4 a -> Tuple4 a -> Int -> Matrix3 a
subMatrix4FromTuple4s (Tuple4 _ x1 y1 z1) (Tuple4 _ x2 y2 z2) (Tuple4 _ x3 y3 z3) 0 = matrix3 x1 y1 z1 x2 y2 z2 x3 y3 z3
subMatrix4FromTuple4s (Tuple4 x1 _ y1 z1) (Tuple4 x2 _ y2 z2) (Tuple4 x3 _ y3 z3) 1 = matrix3 x1 y1 z1 x2 y2 z2 x3 y3 z3
subMatrix4FromTuple4s (Tuple4 x1 y1 _ z1) (Tuple4 x2 y2 _ z2) (Tuple4 x3 y3 _ z3) 2 = matrix3 x1 y1 z1 x2 y2 z2 x3 y3 z3
subMatrix4FromTuple4s (Tuple4 x1 y1 z1 _) (Tuple4 x2 y2 z2 _) (Tuple4 x3 y3 z3 _) 3 = matrix3 x1 y1 z1 x2 y2 z2 x3 y3 z3

subMatrix4 :: Matrix4 a -> Int -> Int -> Matrix3 a
subMatrix4 (Matrix4 _ a b c) 0 n = subMatrix4FromTuple4s a b c n
subMatrix4 (Matrix4 a _ b c) 1 n = subMatrix4FromTuple4s a b c n
subMatrix4 (Matrix4 a b _ c) 2 n = subMatrix4FromTuple4s a b c n
subMatrix4 (Matrix4 a b c _) 3 n = subMatrix4FromTuple4s a b c n

minor3 :: Num a => Matrix3 a -> Int -> Int -> a
minor3 = bunting determinant2 subMatrix3

cofactor3 :: Num a => Matrix3 a -> Int -> Int -> a
cofactor3 matrix m n =
    if odd (m + n) then
        negate (minor3 matrix m n)
    else
        minor3 matrix m n

determinant3 :: Num a => Matrix3 a -> a
determinant3 matrix@(Matrix3 (Tuple3 x y z) _ _)
    = x * cofactor3 matrix 0 0
    + y * cofactor3 matrix 0 1
    + z * cofactor3 matrix 0 2

minor4 :: Num a => Matrix4 a -> Int -> Int -> a
minor4 = bunting determinant3 subMatrix4

cofactor4 :: Num a => Matrix4 a -> Int -> Int -> a
cofactor4 matrix m n =
    if odd (m + n) then
        negate (minor4 matrix m n)
    else
        minor4 matrix m n

determinant4 :: Num a => Matrix4 a -> a
determinant4 matrix@(Matrix4 (Tuple4 x y z w) _ _ _)
    = x * cofactor4 matrix 0 0
    + y * cofactor4 matrix 0 1
    + z * cofactor4 matrix 0 2
    + w * cofactor4 matrix 0 3

invertible :: (Eq a, Num a) => Matrix4 a -> Bool
invertible = (/=0) . determinant4

cofactorsTransposedByDet :: Fractional a => Matrix4 a -> a -> [a]
cofactorsTransposedByDet m d =
    [cofactor4 m c r / d | r <- [0..3], c <- [0..3]]

inverse :: Fractional a => Matrix4 a -> Matrix4 a
inverse =
    matrix4FromList . starling cofactorsTransposedByDet determinant4
