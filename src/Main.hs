import qualified Data.ByteString as B

import Color (RGB(..))
import Vector (Vector(..))
import Shape (AnyShape(..))
import Sphere (Sphere(..))
import Camera (cameraLookAt)
import Tracer (Scene(..), trace)

writePPM :: FilePath -> Integer -> Integer -> [RGB] -> IO ()
writePPM file width height colors = do
  writeFile file $ "P6\n" ++ (show width) ++ " " ++ (show height) ++ "\n255\n"
  B.appendFile file $ B.pack $ concatMap (\(RGB r g b) -> [r, g, b]) colors

main = do
  let (w, h) = (400, 300)
  let sphere1 = (AnyShape $ Sphere (Vector 0 0 5) 3, RGB 255 0 0)
  let sphere2 = (AnyShape $ Sphere (Vector 3 0 5) 2.5, RGB 0 0 255)
  let camera = cameraLookAt (Vector 0 0 0) (Vector 0 0 5)
  let scene = Scene camera [sphere1, sphere2] (RGB 0 0 0)
  let colors = trace scene w h
  writePPM "test.ppm" w h colors
  putStrLn "Hello, world!"
