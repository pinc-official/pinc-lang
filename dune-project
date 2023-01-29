(lang dune 3.6)

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
  ocaml
  dune))
