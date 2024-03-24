.PHONY: all build dev install update fmt fmt-check test test-update test-coverage clean clear
.SILENT: all build dev install update fmt fmt-check test test-update test-coverage clean clear

all: build

build:
	opam exec -- dune build @default

dev:
	opam exec -- dune build --watch

install:
	if ! [ -e _opam ]; then \
		opam switch create . --empty ; \
	fi
	opam install . --deps-only --with-test --yes
	opam lock .

update:
	opam update
	opam upgrade

fmt:
	dune build @fmt --auto-promote

fmt-check:
	dune build @fmt

test:
	dune runtest

test-update:
	dune runtest --auto-promote

test-coverage:
	if [ -d /tmp/pinc-official ]; then rm -r /tmp/pinc-official; fi
	mkdir -p /tmp/pinc-official
	BISECT_FILE=/tmp/pinc-official/pinc-lang dune runtest --no-print-directory --instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path /tmp/pinc-official
	bisect-ppx-report summary --coverage-path /tmp/pinc-official

clean:
	rm -rf _build

clear: clean
	rm -rf _opam

