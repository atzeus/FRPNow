To run the examples
(Note : It seems SDL 1.2 on OS X is broken :( Please try on Linux on Windows))

* Install libsdl (https://www.libsdl.org/). On debian/ubuntu this is sudo apt-get instal libsdl1.2-dev 
* cabal install SDL (https://hackage.haskell.org/package/SDL)
* (from base dir) ghc Examples/Draw (or Examples/TimeFlows)

To replicate measurements shown in paper:

(Also works on OSX)

* ghc -prof Examples/TimeTest
* ./TimeTest +RTS -hy
* convert heap profile to ps with hp2ps
* repeat with optimization off (see Impl/FRPNow.hs on how to do this)
