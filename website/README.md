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

### Serve the Site

```
# from this directory (website/)
make serve
```

This will fire up a web server at http://localhost:4000 by default. To access it from other machines, add `--host ::` (all interfaces, including IPv6).

To automatically rebuild the site by watching the filesystem for changes, run:

```
make watch
```

It takes a few seconds, but just refresh the page to see changes.

## Publishing the Website

On each commit, [Travis](https://travis-ci.org/facebook/flow) builds the site and then [pushes the results to S3](https://github.com/facebook/flow/blob/master/resources/travis/deploy.sh).
