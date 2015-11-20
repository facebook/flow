#!/bin/bash

if [[ "$OSTYPE" == "darwin"* ]]; then
  _canonicalize_file_path() {
    local dir file
    dir=$(dirname -- "$1")
    file=$(basename -- "$1")
    (cd "$dir" 2>/dev/null && printf '%s/%s\n' "$(pwd -P)" "$file")
  }
  FLOW=$(_canonicalize_file_path "$1")
else
  FLOW=$(readlink -f "$1")
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
    dir=$1
    name=${dir%*/}
    name=${name##*/}

    printf "%b[✗] FAIL:%b %s%b\n" \
      $COLOR_RED_BOLD $COLOR_DEFAULT "$1" $COLOR_RESET

    diff_file="${dir}${name}.diff"
    err_file="${dir}${name}.err"
    cat "$err_file"
    if [ -t 1 ] ; then
        esc=$(echo -e "\x1b")
        sed \
            "s/^-/${esc}[31m-/;s/^+/${esc}[32m+/;s/^@/${esc}[35m@/;s/$/${esc}[0m/" \
            < "$diff_file"
    else
        cat "$diff_file"
    fi
}
print_skip() {
    name=$1
    name=${name%*/}
    name=${name##*/}
    printf "%b[-] SKIP:%b %s%b\n" \
      $COLOR_YELLOW_BOLD $COLOR_DEFAULT "$name" $COLOR_RESET
}
print_success() {
    name=$1
    name=${name%*/}
    name=${name##*/}
    printf "%b[✓] PASS:%b %s%b\n" \
      $COLOR_GREEN_BOLD $COLOR_DEFAULT "$name" $COLOR_RESET
}
print_run() {
    name=$1
    name=${name%*/}
    name=${name##*/}
    printf "%b[ ] RUN:%b  %s%b\r" \
      $COLOR_DEFAULT_BOLD $COLOR_DEFAULT "$name" $COLOR_RESET
}
kill_server() {
  trap - SIGINT SIGTERM
  "$FLOW" stop . 1> /dev/null 2>&1
  kill "$1" $$
}
cd "$(dirname "${BASH_SOURCE[0]}")" || exit 1
passed=0
failed=0
skipped=0
filter="$2"

RUNTEST_SUCCESS=0
RUNTEST_FAILURE=1
RUNTEST_SKIP=2
RUNTEST_MISSING_FILES=3

# This function runs in the background so it shouldn't output anything to
# stdout or stderr. It should only communicate through its return value
runtest() {
    return_status=$RUNTEST_SKIP
    dir=$1
    dir=${dir%*/}
    cd "$dir" || exit 1
    name=${dir##*/}
    exp_file="${name}.exp"
    # On Windows we skip some tests as symlinks not available
    if [ "$OSTYPE" = "msys" ]  &&
      ([ $name = "symlink" ] ||
       [ $name = "node_tests" ])
    then
        return $RUNTEST_SKIP
    elif [[ -z $filter || $name =~ $filter ]]
    then
        if ([ ! -e "$exp_file" ] || [ ! -e ".flowconfig" ])
        then
            cd ../.. || exit 1
            if [ "$name" = "auxiliary" ] ||
               [ "$name" = "callable" ] ||
               [ "$name" = "suggest" ]
            then
                return $RUNTEST_SKIP;
            else
                return $RUNTEST_MISSING_FILES;
            fi
        fi

        # check this dir
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
        stderr_dest="$err_file"
        if [ -e ".testconfig" ]
        then
            # all
            if [ "$(awk '$1=="all:"{print $2}' .testconfig)" == "false" ]
            then
                all=""
            fi
            # ignore_stderr
            if [ "$(awk '$1=="ignore_stderr:"{print $2}' .testconfig)" == "false" ]
            then
                stderr_dest="$out_file"
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
            "$FLOW" check . $all --strip-root --show-all-errors --old-output-format 1> "$out_file" 2> "$stderr_dest"
        else
            # otherwise, run specified flow command, then kill the server

            trap "kill_server -INT" SIGINT
            trap "kill_server -TERM" SIGTERM

            # start server and wait
            "$FLOW" start . $all --wait > /dev/null 2>&1
            if [ "$shell" != "" ]
            then
                # run test script
                sh "$shell" "$FLOW" 1> "$out_file" 2> "$stderr_dest"
            else
            # If there's stdin, then direct that in
            # cmd should NOT be double quoted...it may contain many commands
            # and we do want word splitting
                if [ "$stdin" != "" ]
                then
                    cmd="$FLOW $cmd < $stdin 1> $out_file 2> $stderr_dest"
                else
                    cmd="$FLOW $cmd 1> $out_file 2> $stderr_dest"
                fi
                eval "$cmd"
            fi
            # stop server
            "$FLOW" stop . 1> /dev/null 2>&1

            trap - SIGINT SIGTERM
        fi
        diff_file="${name}.diff"
        diff -u --strip-trailing-cr "$exp_file" "$out_file" > "$diff_file"
        if [ -s "$diff_file" ]
        then
            return_status=$RUNTEST_FAILURE
        else
            rm -f "$out_file"
            rm -f "$err_file"
            rm -f "$diff_file"
            return_status=$RUNTEST_SUCCESS
        fi
    else
        return_status=$RUNTEST_SKIP
    fi
    cd ../.. || exit 1
    return $return_status
}


num_to_run_in_parallel=${FLOW_RUNTESTS_PARALLELISM-16}
printf "Running up to %d test(s) in parallel\n" $num_to_run_in_parallel

# Index N of pids should correspond to the test at index N of dirs
dirs=(tests/*/)
pids=()

# Starts running a test in the background. If there are no more tests then it
# does nothing
next_test_index=0
start_test() {
    if (( next_test_index < ${#dirs[@]} )); then
        test_dir="${dirs[next_test_index]}"
        runtest "$test_dir" &
        pids[$next_test_index]=$!
    fi
    ((next_test_index++))
}

# Kick off a bunch of test runs
for ignore_me in $(seq $num_to_run_in_parallel); do
  start_test
done

next_test_to_reap=0
while (( next_test_to_reap < ${#dirs[@]} )); do
    # We reap the tests in order, so that we can output them in a pretty way.
    testname="${dirs[next_test_to_reap]}"
    print_run "$testname"
    wait "${pids[$next_test_to_reap]}"

    case $? in
      $RUNTEST_SUCCESS )
        (( passed++ ))
        print_success "$testname" ;;
      $RUNTEST_FAILURE )
        (( failed++ ))
        print_failure "$testname" ;;
      $RUNTEST_SKIP )
        (( skipped++ ))
        print_skip "$testname" ;;
      $RUNTEST_MISSING_FILES )
        (( failed++ ))
        print_failure "$testname"
        printf "Missing %s.exp file or .flowconfig file\n" "$testname" ;;
    esac

    ((next_test_to_reap++))

    # Start up the next test
    start_test
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
