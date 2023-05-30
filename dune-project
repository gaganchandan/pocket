(lang dune 3.4)

(name pocket)

(using menhir 2.1)

(generate_opam_files true)

(source
 (github gaganchandan/pocket))

(authors "Gagan Chandan")

(maintainers "Gagan Chandan")

(package
 (name pocket)
 (synopsis "A minimal, statically typed, procedural programming language.")
 (depends ocaml dune))
