(lang dune 3.0)
(using menhir 2.1)
(name hals)
(version 0.1.0)

(generate_opam_files true)

(source (github Zaneham/hal-s-compiler))
(license GPL-2.0-or-later)
(authors "Zane Hambly")
(maintainers "Zane Hambly")

(package
 (name hals)
 (synopsis "HAL/S compiler - the Space Shuttle programming language")
 (description "A modern implementation of the HAL/S programming language,
originally developed by Intermetrics for NASA's Space Shuttle flight software.")
 (depends
  (ocaml (>= 4.14))
  dune
  menhir
  ppx_deriving
  (alcotest :with-test)))
