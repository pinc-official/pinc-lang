(lang dune 3.1)

(using action-plugin 0.1)

(name pinc-lang)

(cram enable)

(generate_opam_files true)

(source
 (github pinc-lang/pinc-lang))

(license MIT)

(authors "Torben Ewert")

(maintainers "torben@ewert-online.com")

(package
 (name pinc-lang)
 (synopsis "The PiNC Language as Syntax and Interpreter")
 (depends
  (ocaml
   (and
    (>= 4.14.0)
    (< 5.0.0)))
  dune
  ocaml-lsp-server
  ocamlformat))

(package
 (allow_empty)
 (name pinc-lang-js)
 (synopsis
  "The PiNC Language as Syntax and Interpreter compiled to JavaScript")
 (depends
  (ocaml
   (and
    (>= 4.14.0)
    (< 5.0.0)))
  melange
  dune))
