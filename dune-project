(lang dune 3.11)

(name wascaml)

(generate_opam_files true)

(source
 (github momeemt/wascaml))

(authors "Mutsuha Asada")

(maintainers "")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name wascaml)
 (synopsis "OCaml WebAssembly toolings")
 (description "Ocaml WebAssembly toolings")
 (depends ocaml dune)
 (tags
  (ocaml webassembly compiler sanitizer)))
