PATH=$PATH:~/.cabal/bin
export path
idris +RTS -K4096000000 -RTS --log 0 --build elementalmagicks.ipkg
