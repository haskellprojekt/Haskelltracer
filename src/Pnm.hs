module Pnm (writePNMHeader,
            writeTileToFile,
            saveAsPNG
           ) where
--
-- first: writePNMHeader; then writeTileToFile as often as you like
-- optional: saveAsPNG
--
import System.IO
import Haskelltracer
import qualified Data.ByteString as B
import System.Process

-- beim 101. Byte geht der Binärstream los
bINARYoFFSET :: Int
bINARYoFFSET = 100


tileToByteString :: Tile -> B.ByteString
tileToByteString (Tile {tPixels=rows}) = B.pack(concat(map fromColor23List (concat (rows)))) -- concatMap?

tileToArray :: Tile -> [Byte]
tileToArray (Tile {tPixels=rows}) = concat(map fromColor23List (concat (rows)))


-- schreibt Header mit genügend Pufferkommentaren für spätere Größenänderung, festes Byteoffset für einfügen von Regionen
-- Kommentare nur direkt vor dem Bytestream
-- @TODO: durch Benutzung von ReadWriteMode kann auch der Header zum Schluss geschrieben werden
writePNMHeader :: String -> (Width, Height) -> IO ()
writePNMHeader fn (width, height) = do
                                    h <- openFile fn WriteMode
                                    let header = longheader("P6\n" ++ (show width) ++ " " ++ (show height) ++ "\n" ++ "# puffer comment\n") ++ "\n255\n"
                                    hPutStr h header
                                    hClose h
                                    where
                                    longheader s = if (length s == bINARYoFFSET-5) then s else longheader (s++"#")


-- macht Seek zur richtigen Position und schreibt Binärdaten
-- bevor Bild fertig ist, muss überall hingeschrieben worden sein
-- ansonsten könnte auch hier zum Ende geseekt werden. Oder man macht immer letztes Rechteck als erstes, sodass ein Preview möglich ist.
writeTileToFile :: String -> Tile -> IO ()
writeTileToFile fn r = do
                         h <- openFile fn ReadWriteMode
                         let offset = toInteger (bINARYoFFSET + (getTilePosY r)*(getScreenWidth r)*3 + (getTilePosX r)*3)
                         hSeek h AbsoluteSeek offset
                         foreach h r (tileToByteLines r)
                         hClose h
                         where
                         foreach h r (x:xs) = do
                                        B.hPut h x
                                        hSeek h RelativeSeek (toInteger (distance r))
                                        if xs /= [] then foreach h r xs else hSeek h RelativeSeek 0
                         distance r = ((getScreenWidth r)-(getTileWidth r))*3

tileToByteLines :: Tile -> [B.ByteString]
tileToByteLines (Tile {tPixels=rows}) = map (\ r -> B.pack(concat(map fromColor23List r))) rows


-- kopiert als PNG
saveAsPNG :: String -> IO ProcessHandle
saveAsPNG s = do
              runCommand ("pnmtopng \"" ++ s ++ "\" > \"`dirname \"" ++ s ++ "\"`/`basename \"" ++ s ++ "\" .pnm`.png\"")

--besuretodelete s = do
--                   let x = doesFileExist s
--                   if x then (removeFile s)
