#!/bin/bash -e

# make sure -x (debugging) is off so we don't print the token in the logs
set +x

PLATFORM=$(uname -s || echo unknown)
ARCH=$(uname -m || echo unknown)
INSTALL_DIR="$HOME/.flow_cache/ocaml-${OCAML_VERSION}_opam-${OPAM_VERSION}_${PLATFORM}-${ARCH}"
export PATH="$INSTALL_DIR/usr/bin:$PATH"
export OPAMROOT="$INSTALL_DIR/.opam"
eval "$(opam config env)"

printf "travis_fold:start:installing_opam_publish\nInstalling opam-publish\n"
opam install opam-publish -y
printf "travis_fold:end:installing_opam_publish\n"

mkdir -p "$HOME/opam-publish"
pushd "$HOME/opam-publish" > /dev/null

mkdir -p "$OPAMROOT/plugins/opam-publish"
echo -n "${DOC_BOT_TOKEN}" > "$OPAMROOT/plugins/opam-publish/flow-bot.token"

printf "travis_fold:start:prepare_opam_release\nPreparing opam release\n"
opam publish prepare "https://github.com/${TRAVIS_REPO_SLUG}/archive/${TRAVIS_TAG}.tar.gz"
printf "travis_fold:end:prepare_opam_release\n"
printf "travis_fold:start:submit_opam_release\nSubmitting opam release\n"
opam publish submit --name=flow-bot
printf "travis_fold:end:submit_opam_release\n"

rm "$OPAMROOT/plugins/opam-publish/flow-bot.token"

popd > /dev/null
