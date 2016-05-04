Since we need the .o formats for the idirs, we have to compile these also.

New stuff to compile:
gcc -g -O -c writer.c
gcc -g -O -c reader.c
idris hello.idr -o idris_program

To test the program, recomiple the exe:
gcc writer.c -o writer
./writer
gcc reader.c -o reader
./reader

// **
// ** If you download this folder you should be able to do the follow commands to run the program
// **

Example running it, from 2 terminal windows:
./writer
Sending Hello 
Received: Hello was received via Idris, the god of languages


./idris_program                                               
Received: Hello
Sending Hello was received via Idris, the god of languages
