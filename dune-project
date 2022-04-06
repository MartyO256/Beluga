(lang dune 3.0)

(name beluga)

(implicit_transitive_deps false)

(generate_opam_files true)

(license "GPL-3.0-only")

(authors
 "Brigitte Pientka"
 "Joshua Dunfield"
 "Andrew Cave"
 "Jacob Thomas Errington"
 "Junyoung Clare Jang")

(maintainers
 "jacob.errington@mail.mcgill.ca"
 "marc-antoine.ouimet@mail.mcgill.ca")

(homepage "http://complogic.cs.mcgill.ca/beluga/")

(source
 (github "Beluga-lang/Beluga"))

(package
 (name beluga)
 (synopsis
  "Implementation of contextual modal logic for reasoning with higher-order abstract syntax")
 (depends
  dune-build-info
  extlib
  gen
  linenoise
  (ocaml
   (>= 4.08))
  sedlex
  (ocamlformat :dev)
  (ocaml-lsp-server :dev)
  hamt))
