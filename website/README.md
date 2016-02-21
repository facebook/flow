# Contributing to flowtype.org

The Flow website is built using [Jekyll](https://jekyllrb.com/).

## Running locally

### Install Flow

Many of our docs are written as JS files, in which comments are converted to markdown and JS is typechecked by Flow and shown as syntax-highlighted code blocks with any Flow errors displayed inline. We use a [custom plugin](https://github.com/facebook/flow/blob/master/website/_plugins/jekyll_flowdoc.rb) to build these `.doc.js` files.

So, Flow must be installed and on your path. See [the instructions](https://github.com/facebook/flow) on GitHub.

### Install Ruby

Many systems ship with a fairly old version; we use 2.2 to build and deploy the site. One way to get a recent version is to use [rvm](https://rvm.io/):

```
gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
\curl -sSL https://get.rvm.io | bash -s stable --ruby=2.2
```

### Install Jekyll and its dependencies

```
# from this directory (website/)
gem install bundler
bundle install
```

### Run Jekyll

```
jekyll serve -w
```

This will fire up a web server at http://localhost:8000 by default. To access it from other machines, add `--host ::` (all interfaces, including IPv6).

The `-w` flag watches the filesystem for changes, so pages will rebuild on save. It takes a few seconds, but just refresh the page to see changes.

## Publishing the Website

On each commit, [Travis](https://travis-ci.org/facebook/flow) builds the site and then pushes the results to the [`gh-pages`](https://github.com/facebook/flow/tree/gh-pages) branch. Then [GitHub Pages](https://pages.github.com/) deploys the changes to http://flowtype.org.
