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
 (synopsis "TOML 1.1 codec")
 (description "TOML 1.1 parser and encoder with Bytesrw streaming support")
 (depends
  (ocaml (>= 4.14.0))
  (bytesrw (>= 0.1.0))
  (uutf (>= 1.0.0))
  (alcotest :with-test)))

(package
 (name tomlt-eio)
 (synopsis "Eio integration for TOML codec")
 (description "Eio bindings for tomlt with proper Eio.Io exception integration")
 (depends
  (ocaml (>= 5.0.0))
  (tomlt (= :version))
  (eio (>= 1.0))))

(package
 (name tomlt-jsont)
 (synopsis "Jsont codecs for TOML tagged JSON format")
 (description "Convert between TOML values and the toml-test tagged JSON format using Jsont codecs")
 (depends
  (ocaml (>= 4.14.0))
  (tomlt (= :version))
  (jsont (>= 0.2.0))
  (jsont-bytesrw (>= 0.2.0))))
