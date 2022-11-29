# use Debian Stretch because it still provides jre8, which s3_website currently
# needs. See https://github.com/laurilehmijoki/s3_website/issues/300 and
# https://github.com/laurilehmijoki/s3_website/pull/329
FROM cimg/ruby:2.6-node
MAINTAINER Flow Team <flow@fb.com>
LABEL org.opencontainers.image.source=https://github.com/facebook/flow
LABEL org.opencontainers.image.licenses=MIT

# s3_website needs Java
RUN sudo apt-get update && sudo apt-get install -y openjdk-8-jre-headless

# install bundler 2.x
RUN sudo gem install bundler
