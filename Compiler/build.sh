alex Lexer.x
happy Parser.y -o Parser.hs
ghc typechecker.hs -o typechecker

echo "1"
./typechecker < test.es
echo "2"
./typechecker < test2.es
echo "3"
./typechecker < test3.es
echo "4"
./typechecker < test4.es
echo "5"
./typechecker < test5.es
echo "6"
./typechecker < test6.es
echo "7"
./typechecker < test7.es
echo "8"
./typechecker < test8.es
echo "9"
./typechecker < test9.es
echo "10"
./typechecker < test10.es
echo "11"
./typechecker < test11.es
echo "12"
./typechecker < test12.es
echo "13"
./typechecker < test13.es
