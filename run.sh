#!/bin/sh
rm test.pnm
rm test.png
dist/build/haskelltracer/haskelltracer +RTS -N -RTS
