#!/bin/bash

# USAGE: run
#     $ ./converage.sh <path_to_flow> <directory_to_test_coverage>
# Note that the directory on which coverage is to be tested should have one
# folder for each library containing all the .d.ts declaration files for
# that library.

if [[ "$OSTYPE" == "darwin"* ]]; then
  FLOW=$(pwd -P)/$1
  ROOT=$(pwd -P)/$2
else
  FLOW=$(readlink -f $1)
  ROOT=$(readlink -f $2)
fi
cd "$(dirname "${BASH_SOURCE[0]}")"
total=0
converted=0
checked=0
for dir in $ROOT/*/
do
    dir=${dir%*/}
    cd $dir
    name=${dir##*/}
    exp_file="${name}.exp"
    echo "Running on directory: ${name}"

    # For each library (folder) we first try to convert all the .d.ts files
    # to Flow declaration files and then try to check if flow pick up the
    # produced declaration files.

    $FLOW convert  --r . >/dev/null 2>&1
    if (( $? == 0 ));
    then
        printf "[libs]\ndeclarations/" >.flowconfig
        rm -rf declarations
        mkdir declarations
        mv *.js declarations/
        (( converted ++))
        $FLOW check --all --strip-root --show-all-errors >/dev/null 2>&1
        if (( $? == 0 ));
        then
            (( checked ++ ))
        fi
    fi
    ((total ++ ))
    cd ../..
done

echo Total : $total
echo converted : $converted
echo Checked by Flow : $checked
