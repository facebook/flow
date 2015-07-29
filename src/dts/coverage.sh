#!/bin/bash

# USAGE: run
#     $ ./converage.sh <path_to_flow> <directory_to_test_coverage>
# Note that the directory on which coverage is to be tested should have one
# folder for each library containing all the .d.ts declaration files for
# that library.
current=$PWD
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
cd $ROOT
printf "[libs]\n" >.flowconfig
printf "// @flow" > test.js
for dir in $ROOT/*/
do
    dir=${dir%*/}
    cd $dir
    name=${dir##*/}
    exp_file="${name}.exp"
    echo "Running on directory: ${name}"

    # For each library (folder) we first try to convert all the .d.ts files
    # to Flow declaration files

    $FLOW convert  --r . >/dev/null 2>&1
    if (( $? == 0 ));
    then
        printf "$dir\n" >>../.flowconfig
        (( converted ++))
    fi
    ((total ++ ))
    cd ../..
done

cd $current

# Run flow check on all the converted declaraion files and run a python script
# on the errors to determine number of files whicha are not picked up by flow

error_count=$($FLOW check --json $ROOT | python unique.py)

sound=$converted
(( sound-=$error_count ))

echo Total : $total
echo Converted : $converted
echo Sound Conversions : $sound
