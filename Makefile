.PHONY: all build install fmt fmt-check test test-update clean clear
.SILENT: all build install fmt fmt-check test test-update clean clear

all: build

build:
	opam exec -- dune build @install

install:
	if ! [ -e _opam ]; then \
	   opam switch create . --empty && \
	   opam install ocaml.4.14.0 ; \
	fi
	opam install ./*.opam --deps-only --with-test \

fmt:
	opam exec -- dune build @fmt --auto-promote

fmt-check:
	opam exec -- dune build @fmt

test:
	opam exec -- dune build @runtest

test-update:
	opam exec -- dune build @runtest --auto-promote

clean:
	rm -rf _build

clear: clean
	rm -rf _opam

