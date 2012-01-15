module Main where
import Haskelltracer
import Plane
import Triangle
import Pnm

data Geometries = T Triangle | P Plane deriving Show
-- hardcoded because even with some extended tricks a mixed list is not possible?

data Scene = Scene { camera :: Camera
,   sGeometries :: [Geometries]
}

main = do
       putStrLn "Hello World!"
