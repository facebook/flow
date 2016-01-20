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

install_opam () {
  printf "travis_fold:start:opam_installer\nInstalling ocaml %s and opam %s\n" "$OCAML_VERSION" "$OPAM_VERSION"
  export PREFIX="./usr"
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
  if ! "$OPAM" repository list -s | grep "\<default\>" > /dev/null; then
    "$OPAM" repository add default https://opam.ocaml.org --priority=-1 >/dev/null
  fi
  "$OPAM" switch "$OCAML_VERSION"
  # TODO: Install js_of_ocaml and test the parser
  # opam install ${OPAM_DEPENDS}
  eval $("$OPAM" config env)
  printf "travis_fold:end:opam_installer\n"

  printf "Using ocaml %s and opam %s\n" $(ocaml -vnum) $("$OPAM" --version)
}

setup_linux () {
  # For some reason the Linux containers start killing the tests if too many
  # tests are run in parallel. Luckily we can easily configure that here
  export FLOW_RUNTESTS_PARALLELISM=4
}

setup_osx () {
  : # Nothing special to do here right now
}

install_opam

case $TRAVIS_OS_NAME in
  osx) setup_osx ;;
linux) setup_linux ;;
esac

printf "travis_fold:start:make\nBuilding flow\n"
make
printf "travis_fold:end:make\n"

printf "travis_fold:start:runtests\nRunning flow tests\n"
./runtests.sh bin/flow
printf "travis_fold:end:runtests\n"
