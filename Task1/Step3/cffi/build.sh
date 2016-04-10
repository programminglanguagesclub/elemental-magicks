cd lib
make
cd ..
cd app
urweb app
cd ..
cd lib
gcc -g -O -c reader.c
idris hello.idr -o idris_program
