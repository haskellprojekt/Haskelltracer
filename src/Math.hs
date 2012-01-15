module Math where
import Haskelltracer

instance Num Vector where
  (+) (Vector a b c) (Vector x y z) = Vector (a+x) (b+y) (c+z)
  fromInteger i = Vector (fromInteger i) (fromInteger i) (fromInteger i)
  -- because of fromInteger: (Vector 1 1 1) == 1 :: Vector
  (-) (Vector a b c) (Vector x y z) = Vector (a-x) (b-y) (c-z)
  abs _ = error "abs is not defined for a vector"
  (*) _ = error "(*) is not defined for a vector"
  signum _ = error "signum is not defined for a vector"

crossproduct :: Vector -> Vector -> Vector
crossproduct (Vector a b c) (Vector x y z) = Vector (b*z - c*y) (c*x - a*z) (a*y - b*x)

scalarproduct :: Vector -> Vector -> Float
scalarproduct (Vector a b c) (Vector x y z) = a*x + b*y + c*z

vangle :: Vector -> Vector -> Float
vangle a b = asin ((vlen (a `crossproduct` b)) / ((vlen a)*(vlen b))) * 180.0 / pi

vlen :: Vector -> Float
vlen (Vector a b c) = sqrt(a*a + b*b + c*c)

vmult :: Vector -> Float -> Vector
vmult (Vector a b c) i = Vector (a*i) (b*i) (c*i)

vdiv :: Vector -> Float -> Vector
vdiv (Vector a b c) i = Vector (a/i) (b/i) (c/i)

unitVector :: Vector -> Vector
unitVector a = a `vdiv` (vlen a)
