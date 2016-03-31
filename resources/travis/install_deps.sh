#!/bin/bash -e

OPAM_DEPENDS="js_of_ocaml.2.7"

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

case "$TRAVIS_OS_NAME" in
  osx)
    printf "travis_fold:start:brew_install\nInstalling brew\n"
    brew update
    brew install aspcud
    printf "travis_fold:end:brew_install\n"
    ;;
esac

printf "travis_fold:start:opam_installer\nInstalling ocaml %s and opam %s\n" \
  "$OCAML_VERSION" "$OPAM_VERSION"

PLATFORM=$(uname -s || echo unknown)
ARCH=$(uname -m || echo unknown)

INSTALL_DIR="$HOME/.flow_cache/ocaml-${OCAML_VERSION}_opam-${OPAM_VERSION}_${PLATFORM}-${ARCH}"
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
pushd website >/dev/null
npm install
node_modules/.bin/bower install
popd >/dev/null
printf "travis_fold:end:npm_install\n"
