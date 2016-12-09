ocamlc -c origami.mli origami.ml test.ml
ocamlc -o test origami.cmo test.cmo
chmod +x test
./test
rm ./test *cmo *cmi
