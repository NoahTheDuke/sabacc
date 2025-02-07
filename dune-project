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
   (ocaml (= 5.3.0))
   (pvec (= 0.1.1))
   (ppx_deriving (= 6.0.3))
   (range (= 0.8.3))
   (alcotest (and :with-test (= 1.8.0)))
   (testo (and :with-test (= 0.1.0)))
   (ocaml-lsp-server (and :dev (= 1.22.0)))
   (ocamlformat (and :dev (= 0.27.0))))
 (tags
  ("add topics" "to describe" your project)))
