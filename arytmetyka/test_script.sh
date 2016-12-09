ocamlc -c arytmetyka.mli arytmetyka.ml arytm_test.ml
ocamlc -o test arytmetyka.cmo arytm_test.cmo
chmod +x test
./test
rm ./test
