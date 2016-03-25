#!/bin/bash -e

OPAM_DEPENDS=

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

setup_linux () {
  printf "travis_fold:start:opam_installer\nInstalling ocaml %s and opam %s\n" "$OCAML_VERSION" "$OPAM_VERSION"
  export PREFIX="$HOME/usr"
  export BINDIR="$PREFIX/bin"
  export PATH="$BINDIR:$PATH"

  file="opam-$OPAM_VERSION-$(uname -m || echo unknown)-$(uname -s || echo unknown)"

  echo Downloading OPAM...
  getopam "https://github.com/ocaml/opam/releases/download/$OPAM_VERSION" "$file"

  mkdir -p "$BINDIR" 2>/dev/null || true
  install -m 755 "$TMP/$file" "$BINDIR/opam"
  rm -f "$TMP/$file"

  OPAM="$BINDIR/opam"

  export OPAMYES=1
  export OPAMVERBOSE=1
  "$OPAM" init -a -y -k local flow resources/opam --comp "$OCAML_VERSION"
  "$OPAM" repository add default https://opam.ocaml.org --priority=-1 >/dev/null
  # TODO: Install js_of_ocaml and test the parser
  # opam install ${OPAM_DEPENDS}
  printf "travis_fold:end:opam_installer\n"
}

setup_osx () {
  case $OCAML_VERSION in
  4.02.1)
    brew update
    brew install ocaml ;;
  4.01.0)
    brew install ocaml ;;
  *) echo "Unknown $OCAML_VERSION"; exit 1 ;;
  esac

  # TODO: Figure out how to get opam to run in travis.
  # 1.2.0 fails due to https://github.com/ocaml/opam/issues/1853
  # 1.1.0 fails due to :
  #  'opam init -a -y' failed.
  #  Fatal error:
  #  Sys_error("/Users/travis/.opam/repo/default/packages/ott/ott.0.21.2/opam: Too many open files")
}

case $TRAVIS_OS_NAME in
  osx) setup_osx ;;
linux) setup_linux ;;
esac

printf "travis_fold:start:npm_install\nInstalling npm dependencies\n"
cd website/ && npm install
cd website/ && node_modules/.bin/bower install
printf "travis_fold:end:npm_install\n"
