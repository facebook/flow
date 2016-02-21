#!/bin/bash -e

# make sure -x (debugging) is off so we don't print the token in the logs
set +x

PAGES_CHECKOUT="$HOME/gh-pages"
export BUNDLE_GEMFILE="${TRAVIS_BUILD_DIR}/website/Gemfile"

printf "travis_fold:start:installing_ruby\nInstalling Ruby\n"
source "$HOME/.rvm/scripts/rvm"
rvm use 2.2 --install --binary
gem install --no-rdoc --no-ri bundler
printf "travis_fold:end:installing_ruby\n"

printf "travis_fold:start:installing_jekyll\nInstalling Jekyll\n"
bundle install
printf "travis_fold:end:installing_jekyll\n"

printf "travis_fold:start:clone_gh_pages\nCloning gh-pages\n"
mkdir -p "$PAGES_CHECKOUT"
# pull the gh-pages branch from $REPO.
# doesn't use `clone` to avoid writing the token to disk
git -C "$PAGES_CHECKOUT" init
git -C "$PAGES_CHECKOUT" checkout -b gh-pages
git -C "$PAGES_CHECKOUT" config user.name "flow-bot"
git -C "$PAGES_CHECKOUT" config user.email "flow-bot+docs@fb.com"
git -C "$PAGES_CHECKOUT" pull -q --depth 20 "https://github.com/${TRAVIS_REPO_SLUG}.git" gh-pages
printf "travis_fold:end:clone_gh_pages\n"

printf "travis_fold:start:jekyll_build\nBuilding Jekyll site\n"
env \
  PATH="${TRAVIS_BUILD_DIR}/bin:$PATH" \
  bundle exec jekyll build -s website/ -d "$PAGES_CHECKOUT"
printf "travis_fold:end:jekyll_build\n"

printf "travis_fold:start:push_gh_pages\nPushing gh-pages\n"
git -C "$PAGES_CHECKOUT" add -f -A .
git -C "$PAGES_CHECKOUT" commit -m "Updating to ${TRAVIS_REPO_SLUG}@${TRAVIS_COMMIT}" || :
git -C "$PAGES_CHECKOUT" push -q "https://${GH_BOT_TOKEN}@github.com/${TRAVIS_REPO_SLUG}.git" gh-pages 2>/dev/null
printf "travis_fold:end:push_gh_pages\n"
