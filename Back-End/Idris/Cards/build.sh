PATH=$PATH:~/.cabal/bin
export path
idris +RTS -K256000000 -RTS --log 0 --build cards.ipkg

