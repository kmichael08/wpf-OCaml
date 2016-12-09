ocamlc -c przelewanka.mli przelewanka.ml test.ml
ocamlc -o test przelewanka.cmo test.cmo
chmod +x test
./test
rm ./test *cmo *cmi
