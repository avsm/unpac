(lang dune 3.18)
(name unpac)
(generate_opam_files true)

(package
 (name unpac)
 (synopsis "Monorepo management tool")
 (description "A tool for managing OCaml monorepos with opam repository integration")
 (authors "Anil Madhavapeddy")
 (license ISC)
 (depends
  (ocaml (>= 5.1.0))
  (eio_main (>= 1.0))
  (logs (>= 0.7.0))
  (fmt (>= 0.9.0))
  tomlt
  (jsont (>= 0.2.0))))

(package
 (name unpac-opam)
 (synopsis "Opam backend for unpac")
 (description "Opam package vendoring backend for unpac")
 (authors "Anil Madhavapeddy")
 (license ISC)
 (depends
  (ocaml (>= 5.1.0))
  unpac
  opam-format
  opam-core
  opam-state
  opam-0install
  (cmdliner (>= 1.2.0))))

(package
 (name unpac-claude)
 (synopsis "Claude AI agent for unpac")
 (description "An autonomous Claude agent that understands unpac workflows and can explore/code in a loop")
 (authors "Anil Madhavapeddy")
 (license ISC)
 (depends
  (ocaml (>= 5.1.0))
  unpac
  claude
  (eio_main (>= 1.0))
  (cmdliner (>= 1.2.0))
  (logs (>= 0.7.0))
  (fmt (>= 0.9.0))
  (digestif (>= 1.0.0))
  (base64 (>= 3.0.0))))
