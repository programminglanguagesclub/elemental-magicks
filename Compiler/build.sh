happy elementalParser.y -o elementalParser.hs
ghc elementalParser.hs -o elementalParser
echo "1"
./elementalParser < test.es
echo "2"
./elementalParser < test2.es
echo "3"
./elementalParser < test3.es
echo "4"
./elementalParser < test4.es
echo "5"
./elementalParser < test5.es
echo "6"
./elementalParser < test6.es
echo "7"
./elementalParser < test7.es
echo "8"
./elementalParser < test8.es
echo "9"
./elementalParser < test9.es
echo "10"
./elementalParser < test10.es
echo "11"
./elementalParser < test11.es
echo "12"
./elementalParser < test12.es
echo "13"
./elementalParser < test13.es
