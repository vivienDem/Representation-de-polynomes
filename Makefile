C_FLAGS=
C=ocamlopt
EXE=polynome arbre experimentations

all: $(EXE)

arbre: polynome.cmx arbre.cmx
	$(C) -o arbre polynome.cmx arbre.cmx

experimentations: polynome.cmx arbre.cmx experimentations.cmx
	$(C) -o experimentations polynome.cmx arbre.cmx experimentations.cmx

%.cmi: %.mli
	ocamlc $(C_FLAGS) -c $^

%.cmx: %.ml
	$(C) $(C_FLAGS) -c $^

%: %.cmx
	$(C) $(C_FLAGS) -o $@ $^

clean:
	rm -f *.{cmx,cmi,o} $(EXE) *~
