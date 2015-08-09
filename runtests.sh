#!/bin/bash
if [[ "$OSTYPE" == "darwin"* ]]; then
  FLOW=$(pwd -P)/$1
else
  FLOW=$(readlink -f $1)
fi
cd "$(dirname "${BASH_SOURCE[0]}")"
passed=0
failed=0
skipped=0
filter="$2"
for dir in tests/*/
do
    dir=${dir%*/}
    cd $dir
    name=${dir##*/}
    exp_file="${name}.exp"
    if [ -e ".flowconfig" ] && [ -e $exp_file ] &&
        [[ -z $filter || $name =~ $filter ]]
    then
        echo "Testing directory: ${name}"
        out_file="${name}.out"
        $FLOW check . --all --strip-root --show-all-errors 1> $out_file
        diff_file="${name}.diff"
        diff $out_file $exp_file > $diff_file
        if [ -s $diff_file ]
        then
            (( failed++ ))
            echo "FAILED: ${name}"
            cat $diff_file
        else
            (( passed++ ))
            echo "PASSED: ${name}"
            rm -f $out_file
            rm -f $diff_file
        fi
    else
        (( skipped++ ))
        echo "Skipping directory: ${name}"
    fi
    cd ../..
done
echo
echo "Passed: ${passed}, Failed: ${failed}, Skipped: ${skipped}"
exit ${failed}
