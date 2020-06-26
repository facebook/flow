#!/bin/bash
apt-get -qq update
apt-get install -qq opam git wget curl build-essential m4 zip > /dev/null
echo | sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
opam init --disable-sandboxing --reinit -ni -y
eval $(opam env)
cd /flow
if [ ! -d _opam ]; then
  opam switch create . 4.07.1 --deps-only -y
fi
eval $(opam env)
make bin/flow dist/flow.zip
mkdir -p bin/linux-aarch64 && cp bin/flow bin/linux-aarch64/flow
