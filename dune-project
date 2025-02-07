(lang dune 3.16)

(name sabacc)

(generate_opam_files true)

(source
 (github noahtheduke/sabacc))

(authors "Noah Bogart <noah.bogart@hey.com>")

(maintainers "Noah Bogart <noah.bogart@hey.com>")

(license MPL-2.0)

(documentation https://url/to/documentation)

(package
 (name sabacc)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   pvec
   ppx_deriving
   (alcotest :with-test)
   (testo :with-test)
   (ocaml-lsp-server :with-dev-setup)
   (ocamlformat :with-dev-setup))
 (tags
  ("add topics" "to describe" your project)))
