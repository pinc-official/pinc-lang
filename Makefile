.PHONY: all build install update fmt fmt-check test test-update clean clear
.SILENT: all build install update fmt fmt-check test test-update clean clear

all: build

build:
	mel build
	dune build -p pinc-lang --profile=release

install:
	if ! [ -e _opam ]; then \
		opam switch create . --empty && \
		opam install ocaml.4.14.0 --yes ; \
	fi
	opam install ./*.opam --deps-only --with-test --yes \

update:
	opam update
	opam upgrade

fmt:
	dune build @fmt --auto-promote

fmt-check:
	dune build @fmt

test:
	dune build @runtest

test-update:
	dune build @runtest --auto-promote

clean:
	mel clean
	rm -rf _build

clear: clean
	rm -rf _opam

