.PHONY: all build dev install update fmt fmt-check benchmark test test-update test-coverage clean clear
.SILENT: all build dev install update fmt fmt-check benchmark test test-update test-coverage clean clear

all: build

build:
	opam exec -- dune build @default

dev:
	opam exec -- dune build --watch

install:
	if ! [ -e _opam ]; then \
		opam switch create . --empty ; \
	fi
	opam install . --deps-only --with-test --with-dev-setup --yes
	opam lock .

update:
	opam update
	opam upgrade

fmt:
	dune build @fmt --auto-promote

fmt-check:
	dune build @fmt

benchmark:
	dune exec -- benchmark

test:
	dune runtest

test-update:
	dune runtest --auto-promote

clean:
	rm -rf _build

clear: clean
	rm -rf _opam

