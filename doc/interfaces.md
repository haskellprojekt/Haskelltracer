Interfaces for the haskelltracer
================================


    Config <->
    Parser <-> Renderer <-> Filewriter

data Config = Config
{   outFile :: String
,   inFile :: String
,   screen :: (Width, Height)
,   tiles  :: [Tile]
}

Config :  { outfile, infile, screen: { x, y}, NoOfTiles }
Scene : { Camera, [Geometry], globalAlpha }

type Intersection = Vector
type Distance = Float

type Vector = (Float, Float, Float)
data Ray = R Vector Vector

Class Geometry
    coloredIntersect :: Geometry -> Ray -> (Color, Distance)
    -- intersectRay :: Ray ->  [ Intersection, Distance ]
    -- Intersections are ordered from first to last apperence on ray.
    -- Important: Do not allow negative distances

-- as instances we want to define: sphere, triangle, plane

type Color = (Word8,Word8,Word8)
data Tile = Tile
{   position :: ( Int, Int)
,   screen :: (Width, Height)
,   size :: (Width, Height)
,   pixels :: [[Color]] -- each list is a row
}

-- FileWriter :: [Tile] -> config -> IO ()

-- Renderer :: Tile -> Scene -> config -> Tile

-- Parser :: Filename -> Scene
