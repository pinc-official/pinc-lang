.PHONY: all build install update fmt fmt-check test test-update clean clear
.SILENT: all build install update fmt fmt-check test test-update clean clear

all: build

build:
	opam exec -- dune build @default

install:
	if ! [ -e _opam ]; then \
		opam switch create . --empty ; \
	fi
	opam install . --locked --working-dir --yes
	opam install -y ocaml-lsp-server ocamlformat
	opam lock .

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

