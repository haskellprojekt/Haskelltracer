module Light(Light(..)) where

import Color (RGB)
import Vector (Vector)

data Light = PointLight Vector Float RGB
