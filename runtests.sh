#!/bin/bash
FLOW=$(readlink -f $1)
cd "$(dirname "${BASH_SOURCE[0]}")"
passed=0
failed=0
skipped=0
for dir in tests/*/
do
    dir=${dir%*/}
    cd $dir
    name=${dir##*/}
    exp_file="${name}.exp"
    if [ -e ".flowconfig" ] && [ -e $exp_file ]
    then
        echo "Testing directory: ${name}"
        out_file="${name}.out"
        if [ -d "node_modules" ]
        then
            module=node
        else
            module=haste
        fi
        $FLOW check . --all --strip-root --module $module 1> $out_file
        diff_file="${name}.diff"
        diff $out_file $exp_file > $diff_file
        if [ -s $diff_file ]
        then
            (( failed++ ))
            echo "FAILED: ${name}"
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
