FROM ocaml/opam2:centos-7-opam
LABEL maintainer="Flow Team <flow@fb.com>"

USER root
RUN yum update -y && yum install -y m4 zip libatomic
USER opam
