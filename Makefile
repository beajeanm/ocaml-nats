.PHONY: all test clean dependencies coverage

default: build

all: clean dependencies install

dependencies:
	opam install . --deps-only --with-test --yes

build:
	dune build
	dune build @fmt --auto-promote

test:
	dune runtest --no-buffer

clean:
	dune clean

coverage: clean
	BISECT_ENABLE=yes dune runtest --no-buffer
	bisect-ppx-report summary

install:
	dune install

uninstall:
	dune uninstall
