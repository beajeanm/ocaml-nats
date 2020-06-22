(lang dune 2.6)

(name nats)

(source (uri git+https://github.com/beajeanm/ocaml-nats.git))
(homepage https://github.com/beajeanm/ocaml-nats)
(bug_reports https://github.com/beajeanm/ocaml-nats/issues)

(license ISC)

(authors "Jean-Michel Bea")
(maintainers "Jean-Michel Bea <code@jm-bea.net>")

(generate_opam_files true)

(package
  (name nats)
  (synopsis "OCaml nats library")
  (description "OCaml nats library")
  (depends
    (ocaml (>= 4.04))
    (dune (and (>= 2.6) :build))
    (angstrom (>= 0.14.1))
    (angstrom-lwt-unix (>= 0.14.1))
    (ppx_deriving (>= 4.4))
    (lwt (>= 5.3))
    (uuidm (>= 0.9))
    (containers (>= 2.8))
    (alcotest (and (>= 1.1) :with-test))
    (dryunit (and (>= 0.5) :with-test))
    (ocamlformat (and (>= 0.14) :dev))
    (bisect_ppx (and (>= 2.0) :dev))))
