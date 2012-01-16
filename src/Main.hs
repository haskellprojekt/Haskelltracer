module Main where
import Haskelltracer
import Plane
import Triangle
import Pnm
import Math
import Renderer

main = do
       let screen1 = (640,480)
       let tile1 = Tile (0,0) screen1 screen1 [[]]
       let conf1 = Config "test.pnm" "test.pov" screen1 [tile1]
       let cam1 = Camera (Vector 0 1 1) (Vector 4 5 6) (Vector 0 1 0)
       let geom1 = T (Triangle (Vector 2 3 1) (Vector 5 7 5) (Vector 4 5 9) green)
       let testscene = Scene cam1 [geom1]
       let results = render testscene conf1
       writePNMHeader (cOutFile conf1) (cScreen conf1)
       writeAllTiles (cOutFile conf1) results
       saveAsPNG (cOutFile conf1)
       putStrLn "Finished."


writeAllTiles :: String -> [Tile] -> IO ()
writeAllTiles fname (r:rs) = do
                        writeTileToFile fname r
                        if rsisempty rs then return () else  writeAllTiles fname rs
                             where
                               rsisempty [] = True
                               rsisemptry (s:ss) = False
