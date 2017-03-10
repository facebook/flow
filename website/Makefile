.PHONY: start install serve build i18n-upload i18n-download

.DEFAULT_GOAL := start
start:
	@make install
	@make serve

install: test-bundler
	@git submodule update --init --recursive
	@bundle install

flow_js:
	(cd ..; make js)

_assets/gen/master/flow.js:
	mkdir -p $(dir $@)
	(cd $(dir $@); ln -sf "../../../../bin/flow.js" "flow.js")

static/master/flowlib:
	mkdir -p $(dir $@)
	(cd $(dir $@); ln -sf "../../../lib" "flowlib")

symlinks: _assets/gen/master/flow.js static/master/flowlib

serve: test-jekyll symlinks
	@bundle exec jekyll serve

build: test-jekyll symlinks
	@bundle exec jekyll build

serve-production: test-jekyll flow_js symlinks
	@JEKYLL_ENV=production bundle exec jekyll serve

build-production: test-jekyll flow_js symlinks
	@JEKYLL_ENV=production bundle exec jekyll build

crowdin-sync: test-crowdin
	@crowdin-cli upload sources --auto-update -b master
	@crowdin-cli download -b master

###
# Misc stuff:
###

BUNDLE_EXISTS := $(shell command -v bundle 2> /dev/null)
JEKYLL_EXISTS := $(shell command -v jekyll 2> /dev/null)
CROWDIN_EXISTS := $(shell command -v crowdin-cli 2> /dev/null)

test-bundler:
ifndef BUNDLE_EXISTS
	$(error bundler is not installed. Run `gem install bundler`)
endif

test-jekyll:
ifndef JEKYLL_EXISTS
	$(error Jekyll is not installed. Run `make install`)
endif

test-crowdin:
ifndef CROWDIN_EXISTS
	$(error Crowdin is not installed. Run `make install`)
endif
ifndef CROWDIN_API_KEY
	$(error CROWDIN_API_KEY is undefined)
endif
