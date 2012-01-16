module Sphere where
import Haskelltracer
import Math

data Sphere = Sphere Vector Float Color deriving Show

instance Geometry Sphere where
  coloredIntersect (Sphere a d col) (Ray s r) = if sI then (col, dist) else (col, 0/0)
    where (sI, dist) = sphereIntersection (Ray s r) (Sphere a d col)


sphereIntersection :: Ray -> Sphere -> (Bool, Distance)
sphereIntersection (Ray s r) (Sphere a d _) = (intersect, dist)
  where
    d1 = (x1*(vlen r)) * (-1)
    d2 = (x2*(vlen r)) * (-1)
    rz z = z /= (0/0) && z /= (4/0) && z > 0
    dist = if (rz d1) && (rz d2) then min d1 d2 else if (rz d1) then d1 else d2
    intersect = if dist /= (0/0) && dist /= (4/0) then True else False
    (x1, x2) = abc (r1^2 +r2^2 + r3^2) ((2*(s1+a1)*r1 + 2*(s2+a2)*r2  + 2*(s3+a3)*r3)) ((s1+a1)^2 + (s2+a2)^2 + (s3+a3)^2 - d^2)
    (a1, a2, a3) = v a
    (r1, r2, r3) = v r
    (s1, s2, s3) = v s
    v (Vector a b c) = (a, b, c)
    abc a b c = (x (-), x (+))
                where x f = (-b `f` sqrt(b^2 - 4*a*c) )/2/a
