module Plane where
import Haskelltracer
import Math

data Plane = Plane Vector Vector Vector Color deriving Show
-- @TODO: Maybe
instance Geometry Plane where
  coloredIntersect (Plane a b c col) (Ray s r) = if pI then (col, dist) else (col, 0/0)
    where (pI, dist) = planeIntersection (Ray s r) (Plane a b c col)


-- x>0 means that the ray is going forward
-- u and v have to construct a plane
-- if u,v or x == NaN (z /= z), then there is also no intersection
planeIntersection :: Ray -> Plane -> (Bool, Distance)
planeIntersection (Ray s r) (Plane a b c _) = (intersect, dist)
  where
    dist = x*(vlen r)
    intersect = if u < 0 || v < 0 || u > 1 || v > 1 || x < 0 || u /= u || v /= v || x /= x then False else True
    (x, u, v) = getSolutionUV (s, r) (a, b, c)
