(lang dune 3.1)

(using action-plugin 0.1)

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
  melange
  dune
  ocaml-lsp-server
  ocamlformat))
