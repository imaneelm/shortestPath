all:
	ocamlc -c phase1.mli phase2.mli
	ocamlc -c phase1.ml phase2.ml
	ocamlc -o phase1 phase1.cmo
	ocamlc -o phase2 phase2.cmo


 

