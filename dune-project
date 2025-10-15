(lang dune 3.17)

(using action-plugin 0.1)

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
  fmt
  (dune
   (= 3.18.1))
  crunch
  containers
  ppx_deriving
  ppx_gen_rec
  (bisect_ppx :with-test)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)))
