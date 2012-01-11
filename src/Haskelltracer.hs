module Haskelltracer where
import Data.Word

type Width = Int
type Height = Int

data Config = Config { outFile :: String
,   cInFile :: String
,   cScreen :: (Width, Height)
,   cTiles  :: [Tile]
}

data Camera = Camera { point :: Vector
,   camLookAt :: Vector
,   camSky :: Vector
}

type Intersection = Vector
type Distance = Float
type Vector = (Float, Float, Float)

data Ray = Ray Vector Vector

class Geometry g where
    -- color of nearest intersect (and distance)
    coloredIntersect :: g -> Ray -> (Color, Distance)
    -- intersectRay :: Ray ->  [ Intersection, Distance ]
    -- Intersections are ordered from first to last apperence on ray.
    -- Important: Do not allow negative distances
-- as instances we want to define: sphere, triangle, plane

type Color = (Word8,Word8,Word8)
data Tile = Tile {   position :: ( Int, Int)
,   tScreen :: (Width, Height)
,   tSize :: (Width, Height)
,   tPixels :: [Color]
}

green = (0 :: Word8, 255 :: Word8, 0 :: Word8)
