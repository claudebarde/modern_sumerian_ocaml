(lang dune 3.16)
; The above stanza specifies the Dune language version being used.


; Declares the use of the Melange compiler with a specific version.
(using melange 0.1)

; Declares the use of the directory-targets extension, useful for managing 
; directory-level build targets.
(using directory-targets 0.1)

; Sets the name of the project.
(name sumerian_conjugator_ocaml)

; Indicates whether OPAM files should be automatically generated.
(generate_opam_files true)

; Provides the source location of the project, typically a GitHub repository.
(source
 (github your-username/sumerian_conjugator_ocaml))

; Lists the authors of the project.
(authors "Your Name <your.email@gmail.com>")

; Specifies the maintainers of the project, which can be different from authors.
(maintainers "Your Name <your.email@gmail.com>")

; Declares the license type under which the project is distributed.
(license MIT)

; Provides a link to the project's documentation.
(documentation https://github.com/your-username/sumerian_conjugator_ocaml)

; The package stanza defines the configuration for an OPAM package.
(package
 (name sumerian_conjugator_ocaml)
 (synopsis "Enter a synopsis of your app here")
 (description "Enter a description of your app here")
; Allows the package to be empty, which can be useful during development.
 (allow_empty) 
; Lists the dependencies of the package along with version constraints.
 (depends
  (dune (>= 3.14))
  (melange (>= 3.0.0-51))
  (ocaml (>= 5.1.1))
  opam-check-npm-deps
  ppx_deriving
  (reason (>= 3.11.0))
  (reason-react (>= 0.13.0))
  reason-react-ppx
  ocamlformat
)
; Tags to categorize or label the package, useful for searches and indexing.
 (tags
  (reasonml, ocaml, melange)))

