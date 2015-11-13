OPAM_DEPENDS=
         
setup_linux () {
  printf "travis_fold:start:opam_installer\nInstalling ocaml %s and opam %s\n" $OCAML_VERSION $OPAM_VERSION
  export PREFIX="./usr"
  export BINDIR="$PREFIX/bin"
  export PATH="$BINDIR:$PATH"

  wget -q -O opam_installer.sh "https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh"
  if [ -n "${OPAM_VERSION:-}" ]; then
    sed -i "s/^VERSION=.*$/VERSION='$OPAM_VERSION'/" opam_installer.sh
  fi

  echo y | sh opam_installer.sh $BINDIR $OCAML_VERSION

  export OPAMYES=1
  export OPAMVERBOSE=1
  opam init -a -y
  # TODO: Install js_of_ocaml and test the parser
  # opam install ${OPAM_DEPENDS}
  eval `opam config env`
  printf "travis_fold:end:opam_installer\n"

  # For some reason the Linux containers start killing the tests if too many
  # tests are run in parallel. Luckily we can easily configure that here
  export FLOW_RUNTESTS_PARALLELISM=4
  export FLOW_SHMDIR=/tmp
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

printf "Using ocaml %s and opam %s\n" $(ocaml -vnum) $(opam --version)

printf "travis_fold:start:make\nBuilding flow\n"
make
printf "travis_fold:end:make\n"

printf "travis_fold:start:runtests\nRunning flow tests\n"
./runtests.sh bin/flow
printf "travis_fold:end:runtests\n"
