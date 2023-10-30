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
	if [ -d /tmp/pinc-lang ]; then rm -r /tmp/pinc-lang; fi
	mkdir -p /tmp/pinc-lang
	BISECT_FILE=/tmp/pinc-lang/pinc-lang dune runtest --no-print-directory --instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path /tmp/pinc-lang
	bisect-ppx-report summary --coverage-path /tmp/pinc-lang

clean:
	rm -rf _build

clear: clean
	rm -rf _opam

