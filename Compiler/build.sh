ghc Main.hs -o Main

#for i in {1..13}
#do
# ./Main < tests/source/$i.es 1> tests/output/$i.out 2> tests/error/$i.err
#done

echo "compiling noschools.es"
./Main < tests/source/noschools.es
echo "compiling earth.es"
./Main < tests/source/earth.es
echo "compiling fire.es"
./Main < tests/source/fire.es
echo "compiling water.es"
./Main < tests/source/water.es
echo "compiling air.es"
./Main < tests/source/air.es
echo "compiling spirit.es"
./Main < tests/source/spirit.es
echo "compiling void.es"
./Main < tests/source/void.es
echo "compiling earthfire.es"
./Main < tests/source/multiearthfire.es
echo "compiling earthwater.es"
./Main < tests/source/multiearthwater.es
echo "compiling earthair.es"
./Main < tests/source/multiearthair.es
echo "compiling earthspirit.es"
./Main < tests/source/multiearthspirit.es
echo "compiling earthvoid.es"
./Main < tests/source/multiearthvoid.es
echo "compiling firewater.es"
./Main < tests/source/multifirewater.es
echo "compiling fireair.es"
./Main < tests/source/multifireair.es
echo "compiling firespirit.es"
./Main < tests/source/multifirespirit.es
echo "compiling firevoid.es"
./Main < tests/source/multifirevoid.es
echo "compiling waterair.es"
./Main < tests/source/multiwaterair.es
echo "compiling waterspirit.es"
./Main < tests/source/multiwaterspirit.es
echo "compiling watervoid.es"
./Main < tests/source/multiwatervoid.es
echo "compiling airspirit.es"
./Main < tests/source/multiairspirit.es
echo "compiling airvoid.es"
./Main < tests/source/multiairvoid.es
echo "compiling spiritvoid.es"
./Main < tests/source/multispiritvoid.es


