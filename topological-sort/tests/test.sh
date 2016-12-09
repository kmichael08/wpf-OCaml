ocamlc -c pMap.mli pMap.ml topol.mli mk371148.ml test.ml
ocamlc -o test pMap.cmo mk371148.cmo test.cmo
chmod +x test
./test
rm ./test *cmo *cmi
