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
 (synopsis "TOML 1.1 codec with streaming support")
 (description
  "TOML 1.1 parser and encoder with Bytesrw streaming support. Includes optional subpackages: tomlt.eio for Eio integration and tomlt.jsont for Jsont codecs.")
 (depends
  (ocaml (>= 4.14.0))
  (bytesrw (>= 0.1.0))
  (uutf (>= 1.0.0))
  (ptime (>= 1.0.0))
  (ptime-clock (>= 1.0.0))
  (alcotest :with-test))
 (depopts
  eio
  jsont))
