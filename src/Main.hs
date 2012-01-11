module Main where
import Haskelltracer
import Plane
import Triangle

-- hardcoded because even with some extended tricks a mixed list is not possibl$
data Scene = Scene { camera :: Camera
,   sPlane :: [Plane]
,   sTriangle :: [Triangle]
}

main = do
       -- folgendes geht nicht...
       --let weltliste = [] :: [Geometry g => g]
       --let welt = ((Triangle (0,0,0) (0,0,0) (0,0,0)) :: Geometry b => b):weltliste
       --let d = Triangle (0,0,0) (0,0,0) (0,0,0)
       --let p = Plane (0,0,0) (0,0,0) (0,0,0) (0,0,0)
       --let w = ((d :: Geometry g => g):[p])
       putStrLn "Hello World!"
