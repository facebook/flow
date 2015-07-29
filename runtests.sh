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
        # check this dir
        echo "Testing directory: ${name}"
        out_file="${name}.out"
        err_file="${name}.err"

        # get config flags.
        # for now this is kind of ad-hoc:
        #
        # 1. default flow command is check. it can be overriden here by supplying
        # the entire command on a line that begins with "cmd:".
        # NOTE: we *do not* handle incremental tests yet. for non-check commands,
        # we assume the command will auto-start a server. we run flow stop after
        # we get a result.
        #
        # 2. the check command normally runs with the --all flag. This can be
        # overriden here with the line "all: false". Anything besides "false"
        # is ignored, and the setting itself is ignored if a command besides
        # check is run.
        #
        cmd="check"
        all=" --all"
        stdin=""
        if [ -e ".testconfig" ]
        then
            # cmd
            config_cmd="$(awk '$1=="cmd:"{$1="";print}' .testconfig)"
            if [ "$config_cmd" != "" ]
            then
                cmd="$config_cmd"
            fi
            # all
            if [ "$(awk '$1=="all:"{print $2}' .testconfig)" == "false" ]
            then
                all=""
            fi
            # stdin
            stdin="$(awk '$1=="stdin:"{print $2}' .testconfig)"
        fi

        # run test
        if [ "$cmd" == "check" ]
        then
            # default command is check with configurable --all
            $FLOW check . $all --strip-root --show-all-errors 1> $out_file 2> $err_file
        else
            # otherwise, run specified flow command, then kill the server

            # If there's stdin, then direct that in
            if [ "$stdin" != "" ]
            then
                $FLOW $cmd < $stdin 1> $out_file 2> $err_file
              else
                $FLOW $cmd 1> $out_file 2> $err_file
            fi
            $FLOW stop . 1> /dev/null 2>&1
        fi
        diff_file="${name}.diff"
        diff -u $exp_file $out_file > $diff_file
        if [ -s $diff_file ]
        then
            (( failed++ ))
            echo "FAILED: ${name}"
            cat $err_file
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
            rm -f $err_file
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
