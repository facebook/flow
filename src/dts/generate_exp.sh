#!/bin/bash
if [[ "$OSTYPE" == "darwin"* ]]; then
  FLOW=$(pwd -P)/$1
else
  FLOW=$(readlink -f $1)
fi
cd "$(dirname "${BASH_SOURCE[0]}")"
filter="$2"
for dir in tests/*/
do
    dir=${dir%*/}
    cd $dir
    name=${dir##*/}
    exp_file="${name}.exp"
    if [ -e ".flowconfig" ] && [[ -z $filter || $name =~ $filter ]]
    then
        echo "Running on directory: ${name}"
        $FLOW convert  --r . >/dev/null 2>&1
        $FLOW check --all --strip-root --show-all-errors 1> $exp_file
        cd declarations
        for filename in *.js; do
            mv $filename "${filename}.exp"
        done
        cd ..
    else
        echo "Skipping directory: ${name}"
    fi
    cd ../..
done
