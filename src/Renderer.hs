module Renderer where
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq

import Haskelltracer
import Math
import Triangle
import Plane
import Sphere


data Geometries = T Triangle | P Plane | S Sphere deriving Show
-- hardcoded because even with some extended tricks a mixed list is not possible?

instance Geometry Geometries where
  coloredIntersect (P plane) ray = coloredIntersect plane ray
  coloredIntersect (T triangle) ray = coloredIntersect triangle ray
  coloredIntersect (S sphere) ray = coloredIntersect sphere ray


data Scene = Scene { camera :: Camera
,   sGeometries :: [Geometries]
}


render :: Scene -> Config -> [Tile]
render scene (Config outFile inFile screen tiles) = (parMap rdeepseq) (trace scene) tiles
-- (parMap rdeepseq)
-- alternatives: rdeepseq rseq rpar (old-style: rwhnf)

trace :: Scene -> Tile -> Tile
trace (Scene cam geometries) (Tile (posx, posy) (scrwidth, scrheight) (width, height) _ ) = Tile (posx, posy) (scrwidth, scrheight) (width, height) rows
  where
    rows = makeLines 1
    makeLines z
      | z == height = (makeRow 1 height):[]
      | otherwise = (makeRow 1 z):(makeLines (z+1))
    makeRow s y
      | s == width = (getPixelOn cam (posx + width-1) (posy + y-1) scrwidth scrheight geometries):[]
      | otherwise = (getPixelOn cam (posx + s-1) (posy + y-1) scrwidth scrheight geometries):(makeRow (s+1) y)

-- @TODO: take only smalest distance
getPixelOn :: Camera -> Int -> Int -> Width -> Height -> [Geometries] -> Color
getPixelOn cam a b sw sh geometries = treffen (-1) geometries grey
  where
    treffen pd [] col = col
    treffen pd (object:wl) acol = let (col,dist) = intersect object in if dist/=(0/0) && dist > 0 && (if pd < 0 then True else dist < pd) then treffen dist wl col else treffen pd wl acol
    intersect obj = coloredIntersect obj (Ray ((getLeinPunkt cam x y) + (getLeinVekt cam x y)) (getLeinVekt cam x y))
    x = (fromIntegral a)/(fromIntegral sw)
    y = (fromIntegral b)/(fromIntegral sh)


-- @TODO: to be written as function of camera
leinwagex = Vector 0 0 6.4
leinwagey = Vector 0 4.8 0
richtung cam = (unitVector ((camLookAt cam) - (camPoint cam))) `vmult` 5

getLeinPunkt :: Camera -> Float -> Float -> Vector
getLeinPunkt cam w h = (startp + (lx `vmult` (lw*w))) + (ly `vmult` (lh*h))
                   where
                   startp = mittelp - ((leinwagex + leinwagey) `vdiv` 2)
                   mittelp = (camPoint cam) - (richtung cam)
                   lx = unitVector leinwagex
                   ly = unitVector leinwagey
                   lw = vlen leinwagex
                   lh = vlen leinwagey

getLeinVekt :: Camera -> Float -> Float -> Vector
getLeinVekt cam  w h = (camPoint cam) - (getLeinPunkt cam w h)
