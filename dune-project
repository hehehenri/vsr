(lang dune 3.0)

(name vsr)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name vsr)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "math" "example")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
