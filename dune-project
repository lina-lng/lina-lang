(lang dune 3.16)
(using menhir 3.0)
(name lina)
(version 0.1.0)

(generate_opam_files true)

(license MIT)
(authors "Sergey Golovin")
(maintainers "sgolovin@example.com")

(package
 (name lina)
 (synopsis "An ML-family language compiling to Lua")
 (description
  "Lina is a statically typed functional language with Hindley-Milner type inference that compiles to Lua")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.14))
  (menhir (>= 20231231))
  (menhirLib (>= 20231231))
  (sedlex (>= 3.2))
  (ppx_deriving (>= 6.0))
  (cmdliner (>= 1.2))
  (linol (>= 0.6))
  (linol-eio (>= 0.6))
  (eio (>= 1.0))
  (eio_main (>= 1.0))
  (yojson (>= 2.0))
  (alcotest (and :with-test (>= 1.7)))
  (ppx_expect (and :with-test (>= 0.16)))
  (ppx_inline_test (and :with-test (>= 0.16)))
  (qcheck (and :with-test (>= 0.21)))
  (qcheck-alcotest (and :with-test (>= 0.21)))))

(formatting
 (enabled_for ocaml))
