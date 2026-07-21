(lang dune 3.18)
; The above stanza specifies the Dune language version being used.


; Declares the use of the Melange compiler with a specific version.
(using melange 0.1)

; Sets the name of the project.
(name modern_sumerian_ocaml)

; Indicates whether OPAM files should be automatically generated.
(generate_opam_files true)

; Provides the source location of the project, typically a GitHub repository.
(source
 (github your-username/modern_sumerian_ocaml))

; Lists the authors of the project.
(authors "Your Name <your.email@gmail.com>")

; Specifies the maintainers of the project, which can be different from authors.
(maintainers "Your Name <your.email@gmail.com>")

; Declares the license type under which the project is distributed.
(license MIT)

; Provides a link to the project's documentation.
(documentation https://github.com/your-username/modern_sumerian_ocaml)

; The package stanza defines the configuration for an OPAM package.
(package
 (name modern_sumerian_ocaml)
 (synopsis "Enter a synopsis of your app here")
 (description "Enter a description of your app here")
; Allows the package to be empty, which can be useful during development.
 (allow_empty) 
; Lists the dependencies of the package along with version constraints.
 (depends
  (dune (>= 3.24))
  (melange (= 7.0.0-55))
  (ocaml (= 5.5.0))
  (opam-check-npm-deps (>= 4.1.0))
  (ppx_deriving (>= 6.1.2))
  (reason (>= 3.18.0))
  (reason-react (= 0.17.0))
  (reason-react-ppx (= 0.17.0))
  (ocamlformat (>= 0.29.0))
)
; Tags to categorize or label the package, useful for searches and indexing.
 (tags
  (reasonml, ocaml, melange)))
