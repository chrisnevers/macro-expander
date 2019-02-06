all: main

main:
	ocamlbuild -I src src/main.native

ex:
	./main.native examples/ex.rkt

clean:
	ocamlbuild -clean
