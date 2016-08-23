C_INCLUDE_PATH=/usr/include/urweb
export C_INCLUDE_PATH
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
gcc -g -O -c idrisFFI.c
cd ..
cd Idris
./clean.sh
./build.sh
