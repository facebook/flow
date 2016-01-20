## OPAM overrides

OPAM points at caml.inria.fr to download compilers. This is an additional point
of failure that can break our Travis builds, so we have here a minimal OPAM
repository that instead points at GitHub tarballs for the versions of ocaml that
we build for. As a plus, it also uses SSL.

To add a new version, copy the relevant compilers/ directory structure from
the [upstream repo](https://github.com/ocaml/opam-repository).
