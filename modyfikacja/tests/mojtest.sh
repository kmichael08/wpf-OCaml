ocamlc -c iSet.mli mk371148.ml mojtest.ml
ocamlc -o test mk371148.cmo mojtest.cmo
chmod +x test
./test
rm ./test *cmo *cmi
