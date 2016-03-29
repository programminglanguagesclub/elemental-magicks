Since we need the .o formats for the idirs, we have to compile these also.</ br>

New stuff to compile:
gcc -g -O -c writer.c
gcc -g -O -c reader.c
idris hello.idr -o idris_program</ br>

To test the program, recomiple the exe:
gcc writer.c -o writer
./writer
gcc reader.c -o reader
./reader</ br>


Example running it, from 2 terminal windows:
./writer
Sending Hello 
Received: Hello was received via Idris, the god of languages</ br>


./idris_program                                               
Received: Hello
Sending Hello was received via Idris, the god of languages</ br>
