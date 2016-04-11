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
idris --log 1 --build elementalmagicks.ipkg
