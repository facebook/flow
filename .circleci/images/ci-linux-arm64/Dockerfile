FROM ocaml/opam2:ubuntu-20.04-opam-arm64
LABEL maintainer="Flow Team <flow@fb.com>"

USER root
RUN apt-get -y upgrade && DEBIAN_FRONTEND=noninteractive apt-get -y install m4 zip
USER opam
