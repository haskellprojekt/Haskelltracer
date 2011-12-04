#!/bin/sh
rm utracer
rm utracer.o
rm utracer.hi
ghc -rtsopts -O --make -threaded utracer.hs
# run: +RTS -N

# Unterschied, obwohl ohne threads:

# ghc utracer.hs
#time ./utracer 
#Fertig
#
#real	0m4.870s
#user	0m4.792s
#sys	0m0.068s

# ./compile.sh
#time ./utracer +RTS -N
#Fertig
#
#real	0m1.744s
#user	0m1.728s
#sys	0m0.200s

# ./compile.sh
#time ./utracer 
#Fertig
#
#real	0m1.362s
#user	0m1.300s
#sys	0m0.048s
