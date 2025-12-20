(lang dune 3.0)
(name tomlt)

(generate_opam_files true)

(license ISC)
(authors "Anil Madhavapeddy <anil@recoil.org>")
(maintainers "Anil Madhavapeddy <anil@recoil.org>")
(homepage "https://tangled.org/@anil.recoil.org/ocaml-tomlt")
(bug_reports "https://tangled.org/@anil.recoil.org/ocaml-tomlt/issues")

(package
 (name tomlt)
 (synopsis "TOML 1.1 codec library for OCaml")
 (description
  "Tomlt is a type-safe TOML 1.1 codec library for OCaml, providing
bidirectional encoding and decoding using a combinator-based approach
inspired by Jsont. The core library provides value types and codec
combinators. Optional subpackages provide I/O support:
- tomlt.bytesrw: Streaming parser/encoder using Bytesrw
- tomlt.eio: Eio integration with system clock
- tomlt.unix: Unix I/O with system clock
- tomlt.jsont: Jsont codecs for toml-test JSON format")
 (depends
  (ocaml (>= 4.14.0))
  (ptime (>= 1.0.0))
  (alcotest :with-test))
 (depopts
  (bytesrw (>= 0.1.0))
  (uutf (>= 1.0.0))
  eio
  jsont))
