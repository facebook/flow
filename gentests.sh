#!/bin/bash
FLOW=$(readlink -f $1)
name=$2
PWD=`pwd`
cd "$(dirname "${BASH_SOURCE[0]}")"
cd tests/$name
touch ".flowconfig"
echo "Test directory: ${name}"
$FLOW check . --all --strip-root > "${name}.exp"
cd $PWD
