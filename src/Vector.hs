module Vector(Vector(..), zeroVector, xNormal, yNormal, zNormal,
              add, negate, subtract, multiply, dot, cross,
              length2, length, distance2, distance, normalize, equal) where

import Prelude hiding (negate, subtract, length)

data Vector = Vector { x :: Float, y :: Float, z :: Float } deriving (Show)

zeroVector :: Vector
zeroVector = Vector 0 0 0

xNormal :: Vector
xNormal = Vector 1 0 0

yNormal :: Vector
yNormal = Vector 0 1 0

zNormal :: Vector
zNormal = Vector 0 0 1

add :: Vector -> Vector -> Vector
add (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

negate :: Vector -> Vector
negate (Vector x y z) = Vector (-x) (-y) (-z)

subtract :: Vector -> Vector -> Vector
subtract v1 v2 = add v1 (negate v2)

multiply :: Vector -> Float -> Vector
multiply (Vector x y z) s = Vector (x * s) (y * s) (z * s)

dot :: Vector -> Vector -> Float
dot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vector -> Vector -> Vector
cross (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

length2 :: Vector -> Float
length2 v = dot v v

length :: Vector -> Float
length = sqrt . length2

distance2 :: Vector -> Vector -> Float
distance2 v1 v2 = length2 (subtract v2 v1)

distance :: Vector -> Vector -> Float
distance v1 v2 = sqrt (distance2 v1 v2)

normalize :: Vector -> Vector
normalize v = multiply v (1 / length v)

equal :: Vector -> Vector -> Bool
equal v1 v2 = distance2 v1 v2 < 1e-5
