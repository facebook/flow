#!/bin/bash -e

OPAM_DEPENDS="ocp-build"

# js_of_ocaml 2.7 is  <  ocaml 4.03
# js_of_ocaml 2.8 is  >= ocaml 4.02
# opam 1.1.1 doesn't have js_of_ocaml 2.8
if [ "$OPAM_VERSION" = "1.1.1" ] || [ "$OCAML_VERSION" = "4.01.0" ]; then
  OPAM_DEPENDS+=" js_of_ocaml.2.7"
else
  OPAM_DEPENDS+=" js_of_ocaml.2.8.1"
fi

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
"$OPAM" init \
  --no-setup --yes --quiet \
  -k local flow resources/opam \
  --comp "$OCAML_VERSION" >/dev/null
eval $("$OPAM" config env)
case "$OPAM_VERSION" in
  1.1.*)
    DEFAULT_OPAM_REPO=https://opam.ocaml.org/1.1
    ;;
  *)
    DEFAULT_OPAM_REPO=https://opam.ocaml.org
    ;;
esac
("$OPAM" repository list --short | grep "\bdefault\b" >/dev/null) || \
  "$OPAM" repository --yes \
    add default "$DEFAULT_OPAM_REPO" --priority=-1 >/dev/null

"$OPAM" repository list

echo "Installing dependencies..."
"$OPAM" install --yes ${OPAM_DEPENDS}

printf "travis_fold:end:opam_installer\n"

printf "travis_fold:start:npm_install\nInstalling npm dependencies\n"
  case "$TRAVIS_OS_NAME" in
    osx)
      # OS X has a modern version of node already
      ;;
    *)
      source $HOME/.nvm/nvm.sh
      nvm use 6
  esac

  printf "Using npm version $(npm --version)\n"

  printf "travis_fold:start:npm_install_tool\nRunning npm install for tool\n"
    npm install | cat
  printf "travis_fold:end:npm_install_tool\n"

  printf "travis_fold:start:npm_install_parser\nRunning npm install for the parser\n"
    pushd src/parser >/dev/null
      npm install | cat
    popd >/dev/null
  printf "travis_fold:end:npm_install_parser\n"
printf "travis_fold:end:npm_install\n"
