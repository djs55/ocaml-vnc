#OBUILDOPTS=--debug+
#CONFOPTS=--enable-library-bytecode --enable-executable-bytecode
PKGNAME=vnc

ifneq "$(DESTDIR)" ""
INSTALL_ARGS := -destdir $(DESTDIR)
endif

.PHONY: configure build install clean uninstall

all: build

configure:
	obuild $(OBUILDOPTS) configure $(CONFOPTS)

build: configure
	obuild $(OBUILDOPTS) build

install: build
	ocamlfind remove $(PKGNAME)
	ocamlfind install $(PKGNAME) $(shell find dist/build/lib-vnc -type f) lib/META $(INSTALL_ARGS)

clean:
	obuild clean

uninstall:
	ocamlfind remove $(PKGNAME)
