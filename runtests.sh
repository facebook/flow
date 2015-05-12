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
        # get config flags
        all=" --all"
        if [ -e ".testconfig" ]
        then
            if [ "$(awk '$1=="all:"{print $2}' .testconfig)" == "false" ]
            then
                all=""
            fi
        fi

        # check this dir
        echo "Testing directory: ${name}"
        out_file="${name}.out"
        $FLOW check . $all --strip-root --show-all-errors 1> $out_file
        diff_file="${name}.diff"
        diff -u $exp_file $out_file > $diff_file
        if [ -s $diff_file ]
        then
            (( failed++ ))
            echo "FAILED: ${name}"
            if [ -t 1 ] ; then
                esc=$(echo -e "\x1b")
                cat $diff_file | sed \
                    "s/^-/${esc}[31m-/;s/^+/${esc}[32m+/;s/^@/${esc}[35m@/;s/$/${esc}[0m/"
            else
                cat $diff_file
            fi
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
