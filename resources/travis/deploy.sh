#!/bin/sh -e
# do not set -x or you may expose the secure token in the logs!

REPO="https://${GH_BOT_TOKEN}@github.com/${TRAVIS_REPO_SLUG}.git"
PAGES_CHECKOUT="$HOME/gh-pages"

printf "travis_fold:start:clone_gh_pages\nCloning gh-pages\n"
mkdir -p "$PAGES_CHECKOUT"
git -C "$PAGES_CHECKOUT" init
# pull the gh-pages branch from $REPO.
# doesn't use `clone` to avoid writing the token to disk
git -C "$PAGES_CHECKOUT" pull -q --depth 20 "$REPO" gh-pages
printf "travis_fold:end:clone_gh_pages\n"

printf "travis_fold:start:jekyll_build\nBuilding Jekyll site\n"
PATH="${TRAVIS_BUILD_DIR}/bin:$PATH" jekyll build -s website/ -d "$PAGES_CHECKOUT"
printf "travis_fold:end:jekyll_build\n"

printf "travis_fold:start:push_gh_pages\nPushing gh-pages\n"
git -C "$PAGES_CHECKOUT" add -f .
git -C "$PAGES_CHECKOUT" commit -m "Updating to ${TRAVIS_REPO_SLUG}@${TRAVIS_COMMIT}" || :
git -C "$PAGES_CHECKOUT" push -q "$REPO" gh-pages
printf "travis_fold:end:push_gh_pages\n"
