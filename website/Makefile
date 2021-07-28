# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.

.PHONY: start install serve build i18n-upload i18n-download

ifeq ($(origin DEST), undefined)
BUILD_FLAGS=
else
BUILD_FLAGS=-d $(DEST)
endif

JEKYLL_ENV ?= development

.DEFAULT_GOAL := start
start:
	@make install
	@make serve

install: test-bundler test-yarn
	@yarn install
	@bundle install

flow_js:
	(cd ..; make js)

static/master/flow.js:
	mkdir -p $(dir $@)
	(cd $(dir $@); cp "../../../bin/flow.js" "flow.js")

static/master/flowlib:
	mkdir -p $(dir $@)
	(cd $(dir $@); cp -r "../../../lib" "flowlib")

copies: static/master/flow.js static/master/flowlib

serve: test-yarn test-jekyll copies
	yarn develop

build: test-yarn test-jekyll copies
	yarn run webpack --mode $(JEKYLL_ENV)
	bundle exec jekyll build $(BUILD_FLAGS)

serve-production: test-yarn test-jekyll copies
	@JEKYLL_ENV=production yarn develop

build-production: test-yarn test-jekyll copies
	@yarn run webpack --mode production
	@JEKYLL_ENV=production bundle exec jekyll build $(BUILD_FLAGS)

# crowdin-sync: test-crowdin
# 	@crowdin-cli upload sources --auto-update -b master
# 	@crowdin-cli download -b master

###
# Misc stuff:
###

BUNDLE_EXISTS := $(shell command -v bundle 2> /dev/null)
YARN_EXISTS := $(shell command -v yarn 2> /dev/null)
# CROWDIN_EXISTS := $(shell command -v crowdin-cli 2> /dev/null)

test-bundler:
ifndef BUNDLE_EXISTS
	$(error bundler is not installed. Run `gem install bundler`)
endif

test-yarn:
ifndef YARN_EXISTS
	$(error yarn is not installed. Run `npm install yarn`)
endif

test-jekyll: test-bundler
	@if ! bundle exec jekyll --version > /dev/null 2>&1; then echo 'jekyll is not installed. Run `make install`'; exit 1; fi

# test-crowdin:
# ifndef CROWDIN_EXISTS
# 	$(error Crowdin is not installed. Run `make install`)
# endif
# ifndef CROWDIN_API_KEY
# 	$(error CROWDIN_API_KEY is undefined)
# endif
