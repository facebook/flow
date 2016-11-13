# Copyright (c) 2013-present, Facebook, Inc.
# All rights reserved.

all:

clean:
	@rm -rf _assets/gen static/master _build _site node_modules .asset-cache

_build/ruby_deps_installed: Gemfile Gemfile.lock
	bundle install
	@mkdir -p _build
	@touch $@

_build/js_deps_installed: package.json yarn.lock
	yarn install --pure-lockfile
	@mkdir -p _build
	@touch $@

_assets/gen/master/flow.js:
	mkdir -p $(dir $@)
	ln -sf "../../../../bin/flow.js" $@

static/master/flowlib:
	mkdir -p $(dir $@)
	ln -sf "../../../lib" $@

deps: _build/ruby_deps_installed _build/js_deps_installed

symlinks: _assets/gen/master/flow.js static/master/flowlib

js:
	(cd ..; make js)

serve: deps symlinks js
	bundle exec jekyll serve

watch: deps symlinks js
	bundle exec jekyll serve -w

.SUFFIXES:
.PHONY: all clean deps js symlinks serve watch
