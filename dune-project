(lang dune 3.18)
(name claude)
(generate_opam_files true)

(package
 (name claude)
 (synopsis "OCaml client library for Claude Code")
 (description "An Eio-based OCaml library for interacting with the Claude CLI using JSON streaming")
 (depends
  ocaml
  eio
  fmt
  logs
  (jsont (>= 0.2.0))
  (jsont_bytesrw (>= 0.2.0))
  (alcotest :with-test)))
