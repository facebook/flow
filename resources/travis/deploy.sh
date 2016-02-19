#!/bin/bash -e
# do not set -x or you may expose the secure token in the logs!

PAGES_CHECKOUT="$HOME/gh-pages"

printf "travis_fold:start:installing_ruby\nInstalling Ruby\n"
source "$HOME/.bashrc"
rvm use 2.2 --install --binary
printf "travis_fold:end:installing_ruby\n"

printf "travis_fold:start:installing_jekyll\nInstalling Jekyll\n"
gem install --no-rdoc --no-ri github-pages
printf "travis_fold:end:installing_jekyll\n"

printf "travis_fold:start:clone_gh_pages\nCloning gh-pages\n"
mkdir -p "$PAGES_CHECKOUT"
# pull the gh-pages branch from $REPO.
# doesn't use `clone` to avoid writing the token to disk
git -C "$PAGES_CHECKOUT" init
git -C "$PAGES_CHECKOUT" checkout -b gh-pages
git -C "$PAGES_CHECKOUT" config user.name "facebook-github-bot-0"
git -C "$PAGES_CHECKOUT" config user.email "githubbot+0@fb.com"
git -C "$PAGES_CHECKOUT" pull -q --depth 20 "https://github.com/${TRAVIS_REPO_SLUG}.git" gh-pages
printf "travis_fold:end:clone_gh_pages\n"

printf "travis_fold:start:jekyll_build\nBuilding Jekyll site\n"
PATH="${TRAVIS_BUILD_DIR}/bin:$PATH" jekyll build -s website/ -d "$PAGES_CHECKOUT"
printf "travis_fold:end:jekyll_build\n"

printf "travis_fold:start:push_gh_pages\nPushing gh-pages\n"
git -C "$PAGES_CHECKOUT" add -f .
git -C "$PAGES_CHECKOUT" commit -m "Updating to ${TRAVIS_REPO_SLUG}@${TRAVIS_COMMIT}" || :
git -C "$PAGES_CHECKOUT" push -q "https://${GH_BOT_TOKEN}@github.com/${TRAVIS_REPO_SLUG}.git" gh-pages 2>/dev/null
printf "travis_fold:end:push_gh_pages\n"
