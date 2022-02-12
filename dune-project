(lang dune 2.9)

(cram enable)

(generate_opam_files true)

(source
 (github pinc-lang/pinc-lang))

(license MIT)

(authors "Torben EWert")

(maintainers "torben@ewert-online.com")

(package
 (name pinc-lang)
 (synopsis "The PiNC Language as Syntax an Interpreter")
 (depends
  dune
  (ocaml
   (and
    (>= 4.12.0)
    (< 5.0.0)))))
