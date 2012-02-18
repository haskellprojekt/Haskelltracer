module Utils(maybeZip) where

maybeZip _ [] _ = []
maybeZip _ _ [] = []
maybeZip f (_:xs) (Nothing:ms) = maybeZip f xs ms
maybeZip f (x:xs) (Just a:ms) = f x a : maybeZip f xs ms
