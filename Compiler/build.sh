ghc Main.hs -o Main

for i in {1..13}
do
 ./Main < tests/source/test$i.es 1> tests/output/$i.out 2> tests/error/$i.err
done
