module Plane where
import Haskelltracer

data Plane = Plane Vector Vector Vector Vector deriving Show

instance Geometry Plane where
  coloredIntersect (Plane a b c d) (Ray s r) = (green, 77)
