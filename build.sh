PATH=$PATH:~/.cabal/bin
export path

cd Back-End
cd Glue
make
cd ../..
cd Front-End
cd UrWeb
urweb app
cd ../..
cd Back-End
cd Glue
gcc -g -O -c reader.c
cd ..
cd Idris
./build.sh
