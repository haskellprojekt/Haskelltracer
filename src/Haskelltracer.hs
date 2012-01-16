module Haskelltracer where
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Word

type Byte = Word8
type Width = Int
type Height = Int

data Config = Config { cOutFile :: String
,   cInFile :: String
,   cScreen :: (Width, Height)
,   cTiles  :: [Tile]
}

data Camera = Camera { camPoint :: Vector
,   camLookAt :: Vector
,   camSky :: Vector
}

type Intersection = Vector
type Distance = Float
data Vector = Vector Float Float Float deriving (Show, Eq)

data Ray = Ray Vector Vector

class Geometry g where
    -- color of nearest intersect (and distance)
    coloredIntersect :: g -> Ray -> (Color, Distance)
    -- intersectRay :: Ray ->  [ Intersection, Distance ]
    -- Intersections are ordered from first to last apperence on ray.
    -- Important: Do not allow negative distances
-- as instances we want to define: sphere, triangle, plane

type Color = (Word8,Word8,Word8)
data Tile = Tile {   tPosition :: ( Int, Int)
,   tScreen :: (Width, Height)
,   tSize :: (Width, Height)
,   tPixels :: [[Color]]
} deriving Show

--instance NFData [Tile] where
--  rnf = rdeepseq
  --rnf [] = ()
  --rnf (y:xy) = rnf y `seq` rnf xy
instance NFData Tile where
  --rnf = rwhnf
  rnf x = seq x ()
  --rnf x = par x ()
  --rnf x = seq x ()

getTileWidth :: Tile -> Width
getTileWidth t = fst (tSize t)
getTileHeight :: Tile -> Height
getTileHeight t = snd (tSize t)

getScreenWidth :: Tile -> Width
getScreenWidth t = fst (tScreen t)
getScreenHeight :: Tile -> Height
getScreenHeight t = snd (tScreen t)
getScreen :: Tile -> (Width, Height)
getScreen t = tScreen t

getTilePosX :: Tile -> Int
getTilePosX t = fst (tPosition t)
getTilePosY :: Tile -> Int
getTilePosY t = snd (tPosition t)


fromInt2Color :: [Int] -> Color
fromInt2Color [r, g, b] = (fromIntegral r, fromIntegral g, fromIntegral b)
from3List2Color :: [Word8] -> Color
from3List2Color [r, g, b] = (r, g, b)
fromColor2Int :: Color -> [Int]
fromColor2Int (r, g, b) = [fromIntegral r, fromIntegral g, fromIntegral b]
fromColor23List :: Color -> [Word8]
fromColor23List (r, g, b) = [r, g, b]


green = (0 :: Word8, 255 :: Word8, 0 :: Word8)
grey = (110 :: Word8, 110 :: Word8, 110 :: Word8)
yellow = (0 :: Word8, 255 :: Word8, 255 :: Word8)

