module Plane where
import Haskelltracer

data Plane = Plane Vector Vector Vector Vector Color deriving Show

instance Geometry Plane where
  coloredIntersect (Plane a b c d col) (Ray s r) = (col, 77)
