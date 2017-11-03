#!/bin/bash -e

OPAM_DEPENDS="ocamlfind ocp-build js_of_ocaml.3.0 sedlex.1.99.4"

TMP=${TMPDIR:-/tmp}

dlerror () {
    echo "Couldn't download $url"
    exit 1
}

getopam() {
    opamfile=$2
    url=$1/$opamfile

    if which wget >/dev/null; then
        wget -q -O "$TMP/$opamfile" "$url" || dlerror
    else
        curl -s -L -o "$TMP/$opamfile" "$url" || dlerror
    fi
}

PLATFORM=$(uname -s || echo unknown)
ARCH=$(uname -m || echo unknown)
SLUG="ocaml-${OCAML_VERSION}_opam-${OPAM_VERSION}_${PLATFORM}-${ARCH}"

CACHE_ROOT="$HOME/.flow_cache"
mkdir -p "$CACHE_ROOT"

printf "travis_fold:start:opam_installer\nInstalling ocaml %s and opam %s\n" \
  "$OCAML_VERSION" "$OPAM_VERSION"

INSTALL_DIR="$CACHE_ROOT/$SLUG"
export PREFIX="$INSTALL_DIR/usr"
export OPAMROOT="$INSTALL_DIR/.opam"
BINDIR="$PREFIX/bin"
export PATH="$BINDIR:$PATH"

OPAM="$BINDIR/opam"

if [ -f "$OPAM" ]; then
  echo "Using existing OPAM..."
else
  echo Downloading OPAM...
  file="opam-$OPAM_VERSION-$ARCH-$PLATFORM"
  getopam "https://github.com/ocaml/opam/releases/download/$OPAM_VERSION" "$file"
  mkdir -p "$BINDIR" 2>/dev/null || true
  install -m 755 "$TMP/$file" "$BINDIR/opam"
  rm -f "$TMP/$file"
fi

echo "Initializing OPAM..."
"$OPAM" init --yes --quiet --comp "$OCAML_VERSION" >/dev/null
eval $("$OPAM" config env)
"$OPAM" repository list

echo "Installing dependencies..."
"$OPAM" install --yes ${OPAM_DEPENDS}

echo "opam config:"
echo "INSTALL_DIR=$INSTALL_DIR"
opam config env # print for the logs

eval "$(opam config env)"
echo "Installed packages:"
ocamlfind list

unset PREFIX

printf "travis_fold:end:opam_installer\n"

printf "travis_fold:start:yarn_install\nInstalling yarn dependencies\n"
  case "$TRAVIS_OS_NAME" in
    osx)
      # OS X has a modern version of node already
      export PATH="$PATH:`yarn global bin`"
      ;;
    *)
      source $HOME/.nvm/nvm.sh
      nvm use 6
  esac

  printf "Using yarn version $(yarn --version)\n"

  printf "travis_fold:start:yarn_install_tool\nRunning yarn install for tool\n"
    yarn install | cat
  printf "travis_fold:end:yarn_install_tool\n"

  printf "travis_fold:start:yarn_install_parser\nRunning yarn install for the parser\n"
    pushd src/parser >/dev/null
      yarn install | cat
    popd >/dev/null
  printf "travis_fold:end:yarn_install_parser\n"
printf "travis_fold:end:yarn_install\n"
