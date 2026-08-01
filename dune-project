(lang dune 3.24)

(name chip)

(generate_opam_files true)
(using menhir 3.0)

(source
 (github aram2608/chip))

(authors "Javier Arambula Rascon <arambula.javier7@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name chip)
 (synopsis "Toy scripting language.")
 (description "Port of Lua bytecode-style VM to Ocaml.")
 (depends 
   ocaml
   menhir
   alcotest
   menhirLib
   cmdliner))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
