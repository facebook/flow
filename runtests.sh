#!/bin/bash
if [[ "$OSTYPE" == "darwin"* ]]; then
  FLOW=$(pwd -P)/$1
else
  FLOW=$(readlink -f $1)
fi
if [ -t 1 ]; then
  COLOR_RESET="\x1b[0m"
  COLOR_DEFAULT="\x1b[39;49;0m"
  COLOR_DEFAULT_BOLD="\x1b[39;49;1m"
  COLOR_RED_BOLD="\x1b[31;1m"
  COLOR_GREEN_BOLD="\x1b[32;1m"
  COLOR_YELLOW_BOLD="\x1b[33;1m"
  COLOR_WHITE_ON_RED_BOLD="\x1b[37;41;1m"
else
  COLOR_RESET=""
  COLOR_DEFAULT=""
  COLOR_DEFAULT_BOLD=""
  COLOR_RED_BOLD=""
  COLOR_GREEN_BOLD=""
  COLOR_YELLOW_BOLD=""
  COLOR_WHITE_ON_RED_BOLD=""
fi
print_failure() {
    printf "%b[✗] FAIL:%b %s%b\n" \
      $COLOR_RED_BOLD $COLOR_DEFAULT $1 $COLOR_RESET
}
print_skip() {
    printf "%b[-] SKIP:%b %s%b\n" \
      $COLOR_YELLOW_BOLD $COLOR_DEFAULT $1 $COLOR_RESET
}
kill_server() {
  trap - SIGINT SIGTERM
  $FLOW stop . 1> /dev/null 2>&1
  kill $1 $$
}
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
    if [[ -z $filter || $name =~ $filter ]]
    then
        if ([ ! -e $exp_file ] || [ ! -e ".flowconfig" ])
        then
            cd ../..
            if [ $name = "auxiliary" ] ||
               [ $name = "callable" ] ||
               [ $name = "suggest" ]
            then
                (( skipped++ ))
                print_skip $name
                continue;
            else
                (( failed++ ))
                print_failure $name
                printf "Missing $name.exp file or .flowconfig file\n"
                continue;
            fi
        fi

        # check this dir
        printf "%b[ ] RUN:%b  %s%b\r" \
          $COLOR_DEFAULT_BOLD $COLOR_DEFAULT $name $COLOR_RESET
        out_file="${name}.out"
        err_file="${name}.err"

        # get config flags.
        # for now this is kind of ad-hoc:
        #
        # 1. The default flow command is check with the --all flag. This flag
        # can be overriden here with the line "all: false". Anything besides
        # "false" is ignored, and the setting itself is ignored if a command
        # besides check is run.
        #
        # 2. The default flow command can be overriden here by supplying the
        # entire command on a line that begins with "cmd:". (Note: for writing
        # incremental tests, use the option below). A line beginning with
        # "stdin:" can additionally supply arguments to pass to the command via
        # standard input. We start the server before running the command and
        # stop the server after we get a result.
        #
        # 3. Integration tests that interleave flow commands with file system
        # commands (for testing incremental checks, e.g.) are written by
        # supplying the name of a shell script on a line that begins with
        # "shell:". The shell script is passed the location of the flow binary
        # as argument ($1). We start the server before running the script and
        # stop the server after the script exits.
        #
        all=" --all"
        shell=""
        cmd="check"
        stdin=""
        if [ -e ".testconfig" ]
        then
            # all
            if [ "$(awk '$1=="all:"{print $2}' .testconfig)" == "false" ]
            then
                all=""
            fi
            # stdin
            stdin="$(awk '$1=="stdin:"{print $2}' .testconfig)"
            # shell
            config_shell="$(awk '$1=="shell:"{print $2}' .testconfig)"
            if [ "$config_shell" != "" ]
            then
                cmd=""
                shell="$config_shell"
            fi
            # cmd
            config_cmd="$(awk '$1=="cmd:"{$1="";print}' .testconfig)"
            if [ "$config_cmd" != "" ]
            then
                cmd="$config_cmd"
            fi
        fi

        # run test
        if [ "$cmd" == "check" ]
        then
            # default command is check with configurable --all
            $FLOW check . $all --strip-root --show-all-errors 1> $out_file 2> $err_file
        else
            # otherwise, run specified flow command, then kill the server

            trap "kill_server -INT" SIGINT
            trap "kill_server -TERM" SIGTERM

            # start server and wait
            $FLOW status . 1> /dev/null 2>&1
            if [ "$shell" != "" ]
            then
                # run test script
                sh $shell $FLOW 1> $out_file 2> $err_file
            else
            # If there's stdin, then direct that in
                if [ "$stdin" != "" ]
                then
                    $FLOW $cmd < $stdin 1> $out_file 2> $err_file
                else
                    $FLOW $cmd 1> $out_file 2> $err_file
                fi
            fi
            # stop server
            $FLOW stop . 1> /dev/null 2>&1

            trap - SIGINT SIGTERM
        fi
        diff_file="${name}.diff"
        diff -u $exp_file $out_file > $diff_file
        if [ -s $diff_file ]
        then
            (( failed++ ))
            print_failure $name
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
            printf "%b[✓] PASS:%b %s%b\n" \
              $COLOR_GREEN_BOLD $COLOR_DEFAULT $name $COLOR_RESET
            rm -f $out_file
            rm -f $err_file
            rm -f $diff_file
        fi
    else
        (( skipped++ ))
        print_skip $name
    fi
    cd ../..
done
echo
if [ $failed -eq 0 ]; then
  printf "%bPassed: %d, Failed: %d, Skipped: %d%b\n" \
    $COLOR_DEFAULT_BOLD $passed $failed $skipped $COLOR_RESET
else
  printf "%bPassed: %d, %bFailed: %d%b, Skipped: %d%b\n" \
    $COLOR_DEFAULT_BOLD $passed \
    $COLOR_WHITE_ON_RED_BOLD $failed \
    $COLOR_DEFAULT_BOLD $skipped \
    $COLOR_RESET
fi
exit ${failed}
