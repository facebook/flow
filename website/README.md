# Flow Documentation & Website

We use [Jekyll](http://jekyllrb.com/) to build the site.

## Installation

If you are working on the site, you will want to install and run a local copy
of it.

Start by cloning the website recursively to pull in all submodules:

```sh
git clone git@github.com:facebook/flow.git
```

### Dependencies

In order to use Jekyll, you will need to have Ruby installed. macOS comes
pre-installed with Ruby, but you may need to update RubyGems (via
`gem update --system`). Otherwise, [RVM](https://rvm.io/) and
[rbenv](https://github.com/sstephenson/rbenv) are popular ways to install Ruby.

- [Ruby](http://www.ruby-lang.org/) (version >= 1.8.7)
- [RubyGems](http://rubygems.org/) (version >= 1.3.7)
- [Bundler](http://bundler.io/)

The version of the Pygment syntax highlighter used by Jekyll requires Python
2.7.x (not 3.x). macOS comes pre-installed with Python 2.7, but you may need to
install it on other OSs.

- [Python](https://www.python.org) (version 2.7.x)

Once you have RubyGems and installed Bundler (via `gem install bundler`), use
it to install the dependencies:

```sh
$ cd website
$ bundle install
```

### Instructions

First, make sure to follow the instructions on building Flow in the root
directory of this repository.

Use Jekyll to serve the website locally (by default, at
`http://localhost:8080`):

```sh
$ cd website
$ make
$ open http://127.0.0.1:8080/
```
