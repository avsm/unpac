(lang dune 3.18)

(name claude)

(generate_opam_files true)

(license ISC)
(authors "Anil Madhavapeddy")
(homepage "https://tangled.org/@anil.recoil.org/claudeio")
(maintainers "Anil Madhavapeddy <anil@recoil.org>")
(bug_reports "https://tangled.org/@anil.recoil.org/claudeio/issues")
(maintenance_intent "(latest)")

(package
 (name claude)
 (synopsis "OCaml client library for Claude Code")
 (description "An Eio-based OCaml library for interacting with the Claude CLI using JSON streaming")
 (depends
  (ocaml (>= 5.1.0))
  eio
  eio_main
  fmt
  logs
  cmdliner
  (jsont (>= 0.2.0))
  (odoc :with-doc)
  (alcotest (and :with-test (>= 1.7.0)))))
