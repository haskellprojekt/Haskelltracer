#!/bin/sh
rm utracer
rm utracer.o
rm utracer.hi
ghc -rtsopts -threaded -O3 --make utracer.hs -fforce-recomp
# -prof -auto-all -caf-all

# run: +RTS -N
# und noch mehr optionen zum spielen: -K100M -H800M -qg0 -qb -s -RTS
# -RTS nicht vergessen, wenn später andere argumente kommen

# bei profiling (zweite zeile): -p -hc [oder -hy oder -hd]
# dann: hp2ps -e8in -c utracer.hp

