PATH=/usr/local/bin:/usr/bin:/cygdrive/c/Windows/system32:/cygdrive/c/Windows:/cygdrive/c/Windows/System32/Wbem:/cygdrive/c/Windows/System32/WindowsPowerShell/v1.0
export PATH
OPAMYES=1
export OPAMYES

# print cwd
pwd

# print opam config
opam config list

eval $(opam config env)

# print ocaml config
ocamlc -config

cd "${APPVEYOR_BUILD_FOLDER}"
opam pin add flowtype-ci . -n
opam depext -u flowtype-ci
opam install flowtype-ci --deps-only
make all
make -C src/parser/ ../../_build/src/parser/test/run_tests.native
