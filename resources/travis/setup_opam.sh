PLATFORM=$(uname -s || echo unknown)
ARCH=$(uname -m || echo unknown)
INSTALL_DIR="$HOME/.flow_cache/ocaml-${OCAML_VERSION}_opam-${OPAM_VERSION}_${PLATFORM}-${ARCH}"
export PATH="$INSTALL_DIR/usr/bin:$PATH"
export OPAMROOT="$INSTALL_DIR/.opam"
eval "$(opam config env)"
