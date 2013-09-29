all: build

configure: setup.data

setup.data: setup.ml
	ocaml setup.ml -configure

build: configure
	ocaml setup.ml -build

install: build
	ocaml setup.ml -install

test: build
	./parse_test.native

.PHONY: clean
clean:
	rm -rf _build
