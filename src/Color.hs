module Color(RGB(..), RGBA(..)) where

import Data.Word

data RGB = RGB Word8 Word8 Word8
data RGBA = RGBA Word8 Word8 Word8 Word8
