#!/bin/bash

if [ ! -f ~/.opam/from_cache ]; then
  rm -rf ~/.opam
  opam init --bare --disable-sandboxing --no-setup --yes
fi
