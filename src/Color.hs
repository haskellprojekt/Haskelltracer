module Color(RGB(..), RGBA(..), black, white, red, green, blue) where

import Data.Word

data RGB = RGB Word8 Word8 Word8
data RGBA = RGBA Word8 Word8 Word8 Word8

black :: RGB
black = RGB 0 0 0

white :: RGB
white = RGB 255 255 255

red :: RGB
red = RGB 255 0 0

green :: RGB
green = RGB 0 255 0

blue :: RGB
blue = RGB 0 0 255
