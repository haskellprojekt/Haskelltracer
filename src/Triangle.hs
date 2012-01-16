module Triangle where
import Haskelltracer
import Math

data Triangle = Triangle Vector Vector Vector Color deriving Show

instance Geometry Triangle where
  coloredIntersect (Triangle a b c col) (Ray s r) = if tI then (col, dist) else (col, 0/0)
    where (tI, dist) = triangleIntersection (Ray s r) (Triangle a b c col)


-- x>0 means that the ray is going forward
-- u and v have to construct a triangle
-- if u,v or x == NaN (z /= z), then there is also no intersection
triangleIntersection :: Ray -> Triangle -> (Bool, Distance)
triangleIntersection (Ray s r) (Triangle a b c _) = (intersect, dist)
  where
    dist = x*(vlen r)
    intersect = if u < 0 || v < 0 || (u+v) > 1 || x < 0 || u /= u || v /= v || x /= x then False else True
    (x, u, v) = getSolutionUV (s, r) (a, b, c)
