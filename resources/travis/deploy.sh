#!/bin/bash -e

# make sure -x (debugging) is off so we don't print the token in the logs
set +x

PAGES_CHECKOUT="$HOME/flowtype.org"
export BUNDLE_GEMFILE="${TRAVIS_BUILD_DIR}/website/Gemfile"

case "$TRAVIS_OS_NAME" in
  osx)
    ;; # OS X already has a recent version of node
  *)
    source $HOME/.nvm/nvm.sh
    nvm use 6 # should've been installed by before_install.sh
esac

# Add Yarn to PATH
export PATH="${TRAVIS_BUILD_DIR}/resources/travis/node_modules/.bin:$PATH"

printf "travis_fold:start:installing_ruby\nInstalling Ruby\n"
source "$HOME/.rvm/scripts/rvm"
rvm use 2.3.4 --install --binary
gem install --no-rdoc --no-ri bundler
printf "travis_fold:end:installing_ruby\n"

printf "travis_fold:start:website_deps\nInstalling website deps\n"
(cd website && make install)
printf "travis_fold:end:website_deps\n"

printf "travis_fold:start:jekyll_build\nBuilding Jekyll site\n"
GEN_DIR=$([[ "$TRAVIS_TAG" = "" ]] && echo "master" || echo "$TRAVIS_TAG")
mkdir -p "$PAGES_CHECKOUT"
mkdir -p "website/static/$GEN_DIR"
cp "bin/flow.js" "website/static/${GEN_DIR}/flow.js"
cp -r "lib" "website/static/${GEN_DIR}/flowlib"
echo "version" > "website/_data/flow_dot_js_versions.csv"
git ls-remote --tags 2>/dev/null | awk '{print $2}' | cut -d/ -f3 | \
  grep -e '^v[0-9]\{1,\}\.[0-9]\{1,\}\.[0-9]\{1,\}$' | \
  sort -s -t. -k 1,1nr -k 2,2nr -k 3,3nr | \
  grep -B1000000 '^v0\.37\.0$' >> "website/_data/flow_dot_js_versions.csv"
(cd website && env \
  PATH="${TRAVIS_BUILD_DIR}/bin:$PATH" \
  JEKYLL_ENV=production \
  make build DEST="$PAGES_CHECKOUT")
printf "travis_fold:end:jekyll_build\n"

printf "travis_fold:start:push_s3\nPushing to S3\n"
bundle exec s3_website push --config-dir=website/ --site="$PAGES_CHECKOUT"
printf "travis_fold:end:push_s3\n"
