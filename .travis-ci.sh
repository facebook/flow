OPAM_DEPENDS=
         
case "$OCAML_VERSION,$OPAM_VERSION" in
4.02.0,1.2.0) ppa=avsm/ocaml42+opam12 ;;
4.02.0,1.1.0) ppa=avsm/ocaml42+opam11 ;;
4.01.0,1.2.0) ppa=avsm/ocaml41+opam12 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
4.00.1,1.2.0) ppa=avsm/ocaml40+opam12 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1
opam init 
opam install ${OPAM_DEPENDS}
eval `opam config env`
cd src
make
make test
