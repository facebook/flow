#!/bin/bash

echo "ocaml-$1"
opam --version
gcc -dumpfullversion -dumpversion
cat flowtype.opam
cat flow_parser.opam
cat .circleci/config.yml
