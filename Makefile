all: build

configure: setup.data
	ocaml setup.ml -configure

build: configure
	ocaml setup.ml -build

.PHONY: clean
clean:
	rm -rf _build
