module Triangle where
import Haskelltracer

data Triangle = Triangle Vector Vector Vector deriving Show

instance Geometry Triangle where
  coloredIntersect (Triangle a b c) (Ray s r) = (green, 77)
