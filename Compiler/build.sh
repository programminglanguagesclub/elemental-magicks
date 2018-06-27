ghc Main.hs -o Main

#for i in {1..13}
#do
# ./Main < tests/source/$i.es 1> tests/output/$i.out 2> tests/error/$i.err
#done

./Main < tests/source/noschools.es
./Main < tests/source/earth.es
./Main < tests/source/fire.es
./Main < tests/source/water.es
./Main < tests/source/air.es
./Main < tests/source/spirit.es
./Main < tests/source/void.es
./Main < tests/source/multiearthfire.es
./Main < tests/source/multiearthwater.es
./Main < tests/source/multiearthair.es
./Main < tests/source/multiearthspirit.es
./Main < tests/source/multiearthvoid.es
./Main < tests/source/multifirewater.es
./Main < tests/source/multifireair.es
./Main < tests/source/multifirespirit.es
./Main < tests/source/multifirevoid.es
./Main < tests/source/multiwaterair.es
./Main < tests/source/multiwaterspirit.es
./Main < tests/source/multiwatervoid.es
./Main < tests/source/multiairspirit.es
./Main < tests/source/multiairvoid.es
./Main < tests/source/multispiritvoid.es


