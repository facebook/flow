OPAM_DEPENDS=
         
setup_linux () {
  case "$OCAML_VERSION,$OPAM_VERSION" in
  4.02.1,1.2.0) ppa=avsm/ocaml42+opam12 ;;
  4.02.1,1.1.0) ppa=avsm/ocaml42+opam11 ;;
  4.01.0,1.2.0) ppa=avsm/ocaml41+opam12 ;;
  4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
  *) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
  esac

  export OPAMYES=1
  # We could in theory tell opam to init with a particular version of ocaml,
  # but this is much slower than using the ppa & apt
  opam init -a -y
  # TODO: Install js_of_ocaml and test the parser
  # opam install ${OPAM_DEPENDS}
  eval `opam config env`
}

setup_osx () {
  case $OCAML_VERSION in
  4.02.1)
    brew update
    brew install ocaml ;;
  4.01.0) 
    brew install ocaml ;;
  *) echo Unknown $OCAML_VERSION; exit 1 ;;
  esac

  # TODO: Figure out how to get opam to run in travis.
  # 1.2.0 fails due to https://github.com/ocaml/opam/issues/1853
  # 1.1.0 fails due to :
  #  'opam init -a -y' failed.
  #  Fatal error:
  #  Sys_error("/Users/travis/.opam/repo/default/packages/ott/ott.0.21.2/opam: Too many open files")
}

case $TRAVIS_OS_NAME in
  osx) setup_osx ;;
linux) setup_linux ;;
esac

make
make test
