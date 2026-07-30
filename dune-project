(lang dune 3.24)

(name chip)

(generate_opam_files true)
(using menhir 3.0)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name chip)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   ocaml
   menhir
   menhirLib))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
