.PHONY: test

build:
	dune build

play:
	OCAMLRUNPARAM=b dune exec ./src/game.exe

test:
	OCAMLRUNPARAM=b dune exec ./test/main.exe

install:
	sudo apt update 
	sudo apt install -y libpng-dev libjpeg-dev libtiff-dev libxpm-dev libfreetype-dev libgif-dev
	opam update
	opam upgrade -y
	opam install Graphics -y
	opam install camlimages -y
		
doc: build
	mkdir -p doc
	ocamlfind ocamlc -package graphics -c lvls/levels.ml
	ocamlfind ocamlc -package graphics,camlimages.png,camlimages.graphics -c -I lvls/ src/game.ml
	ocamlfind ocamldoc -I lvls/ -package graphics,camlimages.png,camlimages.graphics \
		-html -stars -d doc lvls/levels.ml src/game.ml

clean:
	ocamlbuild -clean
	rm -f lvls/*.cmi
	rm -f src/*.cmi
	rm -f lvls/*.cmo
	rm -f src/*.cmo
	rm -f lvls/*.o
	rm -f lvls/*.cmx
	rm -rf doc
