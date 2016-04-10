module Main

%include C "reader.h"
%link C "reader.o"

reader : IO String
reader = foreign FFI_C "reader" (IO String)

writer : String -> IO Unit
writer x = foreign FFI_C "writer" (String -> IO Unit) x


main : IO ()
main = do {
x <- reader;
writer (x ++ " was received via Idris, the god of languages");
}
