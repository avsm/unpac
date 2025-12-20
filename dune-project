(lang dune 3.0)
(name tomlt)
(version 0.1.0)

(generate_opam_files true)

(source (github avsm/tomlt))
(license ISC)
(authors "Anil Madhavapeddy <anil@recoil.org>")
(maintainers "Anil Madhavapeddy <anil@recoil.org>")

(package
 (name tomlt)
 (synopsis "TOML 1.1 codec library")
 (description
  "TOML 1.1 type-safe codec library. The core library provides value types and
codec combinators. Optional subpackages provide I/O support:
- tomlt.bytesrw: Streaming parser/encoder using Bytesrw
- tomlt.eio: Eio integration
- tomlt.unix: Unix I/O
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
