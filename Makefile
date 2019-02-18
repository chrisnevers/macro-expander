BUILD_PKGS=
TEST_PKGS=oUnit

BUILD_FLAGS=-Is src
DEBUG_FLAGS=-tag 'debug'
TEST_FLAGS=-use-ocamlfind -pkgs ${TEST_PKGS} -Is src

all: main

main:
	ocamlbuild ${BUILD_FLAGS} src/main.native

test:
	ocamlbuild ${TEST_FLAGS} tests/test.native --

clean:
	ocamlbuild -clean
