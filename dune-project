(lang dune 3.17)

(implicit_transitive_deps false)

(name pinc-lang)

(cram enable)

(generate_opam_files true)

(map_workspace_root false)

(source
 (github pinc-official/pinc-lang))

(license MIT)

(authors "Torben Ewert")

(maintainers "torben@ewert-online.com")

(package
 (name pinc-lang)
 (synopsis "The pinc language as syntax and interpreter")
 (depends
  (ocaml
   (>= 4.12))
  dune
  pprint
  crunch
  ppx_deriving
  js_of_ocaml
  js_of_ocaml-compiler
  js_of_ocaml-ppx
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)))
