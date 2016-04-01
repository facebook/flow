#!/bin/bash -e

if [[ "$TRAVIS_OS_NAME" = "osx" && "$TRAVIS_PULL_REQUEST" = "false" ]]; then

  TMP=${TMPDIR:-/tmp}
  SLUG_PLATFORM=$(uname -s || echo unknown)
  ARCH=$(uname -m || echo unknown)
  SLUG="ocaml-${OCAML_VERSION}_opam-${OPAM_VERSION}_${SLUG_PLATFORM}-${ARCH}"
  CACHE_ROOT="$HOME/.flow_cache"
  CACHE_TAR="$TMP/$SLUG.tar"
  CACHE_TGZ="$CACHE_TAR.gz"

  printf "travis_fold:start:cache.2\nstore build cache\n"
  CHANGED=0
  if [ -f "$CACHE_TAR" ]; then
    tar --update -Pvf "$CACHE_TAR" "$CACHE_ROOT"
    if ! shasum -c "$CACHE_TAR.sha1" >/dev/null; then
      gzip "$CACHE_TAR"
      CHANGED=1
    fi
  else
    tar -Pczf "$CACHE_TGZ" "$CACHE_ROOT"
    CHANGED=1
  fi
  if [ "$CHANGED" -eq 1 ]; then
    echo "uploading $TRAVIS_BRANCH/$SLUG.tar.gz"
    aws s3 cp --storage-class REDUCED_REDUNDANCY "$CACHE_TGZ" "s3://ci-cache.flowtype.org/$TRAVIS_BRANCH/$SLUG.tar.gz"
  else
    echo "nothing changed, not updating cache"
  fi
  printf "travis_fold:end:cache.2\n"
fi
