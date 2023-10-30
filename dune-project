(lang dune 3.7)

(using action-plugin 0.1)

(name pinc-lang)

(cram enable)

(generate_opam_files true)

(map_workspace_root false)

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
   (>= 4.12))
  fmt
  dune
  crunch
  containers
  grpc-eio
  eio_main
  ocaml-protoc
  (bisect_ppx :with-test)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)))
