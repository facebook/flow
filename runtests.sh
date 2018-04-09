#!/bin/bash

show_help() {
  printf "Usage: runtests.sh [-hlqrv] [-d DIR] [-t TEST] [-b] FLOW_BINARY [[-f] TEST_FILTER]\n\n"
  printf "Runs Flow's tests.\n\n"
  echo "    [-b] FLOW_BINARY"
  echo "        path to Flow binary (the -b is optional)"
  echo "    [-f] TEST_FILTER"
  echo "        optional regular expression to choose test directories to run"
  echo "    -l"
  echo "        lists the tests that will be run"
  echo "    -d DIR"
  echo "        run tests in DIR/tests/"
  echo "    -t TEST"
  echo "        run the test DIR/tests/TEST, equivalent to a filter of \"^TEST$\""
  echo "    -r"
  echo "        re-record failing tests to update expected output"
  echo "    -q"
  echo "        quiet output (hides status, just prints results)"
  echo "    -v"
  echo "        verbose output (shows skipped tests)"
  echo "    -h"
  echo "        display this help and exit"
}

export IN_FLOW_TEST=1
export FLOW_LOG_LEVEL=debug

OPTIND=1
record=0
verbose=0
quiet=0
relative="$(dirname "${BASH_SOURCE[0]}")"
list_tests=0
while getopts "b:d:f:lqrt:vh?" opt; do
  case "$opt" in
  b)
    FLOW="$OPTARG"
    ;;
  d)
    relative="$OPTARG"
    ;;
  f)
    filter="$OPTARG"
    ;;
  l)
    list_tests=1
    ;;
  t)
    filter="^$OPTARG$"
    ;;
  q)
    quiet=1
    ;;
  r)
    record=1
    ;;
  v)
    verbose=1
    ;;
  h|\?)
    show_help
    exit 0
    ;;
  esac
done

shift $((OPTIND-1))

[ "$1" = "--" ] && shift

FLOW="${FLOW:-$1}"
filter="${filter:-$2}"

if [[ "$OSTYPE" == "darwin"* ]]; then
  _canonicalize_file_path() {
    local dir file
    dir=$(dirname -- "$1")
    file=$(basename -- "$1")
    (cd "$dir" 2>/dev/null && printf '%s/%s\n' "$(pwd -P)" "$file")
  }
  FLOW=$(_canonicalize_file_path "$FLOW")
else
  FLOW=$(readlink -f "$FLOW")
fi
if [ -t 1 ]; then
  COLOR_RESET="\x1b[0m"
  COLOR_DEFAULT="\x1b[39;49;0m"
  COLOR_DEFAULT_BOLD="\x1b[39;49;1m"
  COLOR_RED_BOLD="\x1b[31;1m"
  COLOR_GREEN_BOLD="\x1b[32;1m"
  COLOR_YELLOW_BOLD="\x1b[33;1m"
  COLOR_MAGENTA_BOLD="\x1b[35;1m"
  COLOR_WHITE_ON_RED_BOLD="\x1b[37;41;1m"
else
  COLOR_RESET=""
  COLOR_DEFAULT=""
  COLOR_DEFAULT_BOLD=""
  COLOR_RED_BOLD=""
  COLOR_GREEN_BOLD=""
  COLOR_YELLOW_BOLD=""
  COLOR_MAGENTA_BOLD=""
  COLOR_WHITE_ON_RED_BOLD=""
fi
print_failure() {
    dir=$1
    name=${dir%*/}
    name=${name##*/}

    if [[ "$record" -eq 1 ]]; then
      printf "%b[✗] UPDATED:%b %s%b\n" \
        "$COLOR_MAGENTA_BOLD" "$COLOR_DEFAULT" "$name" "$COLOR_RESET"
    else
      printf "%b[✗] FAILED:%b  %s%b\n" \
        "$COLOR_RED_BOLD" "$COLOR_DEFAULT" "$name" "$COLOR_RESET"
    fi

    diff_file="${dir}${name}.diff"
    err_file="${dir}${name}.err"
    [ -f "$err_file" ] && cat "$err_file"
    if [ -f "$diff_file" ]; then
      if [ -t 1 ] ; then
        esc=$(echo -e "\x1b")
        sed \
            "s/^-/${esc}[31m-/;s/^+/${esc}[32m+/;s/^@/${esc}[35m@/;s/$/${esc}[0m/" \
            < "$diff_file"
      else
        cat "$diff_file"
      fi
    fi

    if [[ "$record" -eq 1 ]]; then
      mv "${dir}${name}.out" "${dir}${name}.exp"
      rm "$err_file"
      rm "$diff_file"
    fi
}
print_error() {
    dir=$1
    name=${dir%*/}
    name=${name##*/}

    printf "%b[✗] ERRORED:%b %s%b\n" \
      "$COLOR_RED_BOLD" "$COLOR_DEFAULT" "$name" "$COLOR_RESET"

    out_file="${dir}${name}.out"
    [ -f "$out_file" ] && cat "$out_file"

    err_file="${dir}${name}.err"
    [ -f "$err_file" ] && printf "\n\nStderr:\n" && cat "$err_file"

    monitor_log_file="${dir}${name}.monitor_log"
    [ -f "$monitor_log_file" ] && printf "\n\nServer monitor log:\n" && cat "$monitor_log_file"

    log_file="${dir}${name}.log"
    [ -f "$log_file" ] && printf "\n\nServer log:\n" && cat "$log_file"
}
print_skip() {
    name=$1
    name=${name%*/}
    name=${name##*/}
    verbose=$2
    if [[ "$verbose" -eq 1 ]]; then
      printf "%b[-] SKIPPED:%b %s%b\n" \
        "$COLOR_YELLOW_BOLD" "$COLOR_DEFAULT" "$name" "$COLOR_RESET"
    elif [[ -t 1 ]]; then
      printf "             %*s\r" ${#name} " "
    fi
}
print_success() {
    name=$1
    name=${name%*/}
    name=${name##*/}
    printf "%b[✓] PASSED:%b  %s%b\n" \
      "$COLOR_GREEN_BOLD" "$COLOR_DEFAULT" "$name" "$COLOR_RESET"
}
print_run() {
    if [[ -t 1 && "$quiet" -eq 0 ]]; then
      name=$1
      name=${name%*/}
      name=${name##*/}
      printf "%b[ ] RUNNING:%b %s%b\r" \
        "$COLOR_DEFAULT_BOLD" "$COLOR_DEFAULT" "$name" "$COLOR_RESET"
    fi
}

cd "$relative" || exit 1
passed=0
failed=0
skipped=0
errored=0

RUNTEST_SUCCESS=0
RUNTEST_FAILURE=1
RUNTEST_SKIP=2
RUNTEST_MISSING_FILES=3
RUNTEST_ERROR=4

# This function runs in the background so it shouldn't output anything to
# stdout or stderr. It should only communicate through its return value
runtest() {
    dir=$1
    dir=${dir%*/}
    name=${dir##*/}
    exp_file="${name}.exp"
    # On Windows we skip some tests as symlinks not available
    if [ "$OSTYPE" = "msys" ]  &&
      ([ "$name" = "symlink" ] ||
       [ "$name" = "node_tests" ])
    then
        return $RUNTEST_SKIP
    elif [[ -z $filter || $name =~ $filter ]]
    then
        if ([ ! -e "$dir/$exp_file" ] || [[ ! -e "$dir/.flowconfig" && ! -e "$dir/.testconfig" ]])
        then
            if [ "$name" = "auxiliary" ] ||
               [ "$name" = "callable" ] ||
               [ "$name" = "suggest" ]
            then
                return $RUNTEST_SKIP;
            else
                return $RUNTEST_MISSING_FILES;
            fi
        fi

        # Each test should write its output to a unique directory so that
        # parallel runs of the same test don't stomp on each other (Facebook
        # internally runs a stress test to look for flaky tests). If a test
        # fails, we'll then copy the files back to the source directory.
        OUT_PARENT_DIR=$(mktemp -d /tmp/flow_test.XXXXXX)
        OUT_DIR="$OUT_PARENT_DIR/$name"
        mkdir "$OUT_DIR"

        # deletes the temp directory
        function cleanup {
          rm -rf "$OUT_PARENT_DIR"
        }
        function force_cleanup {
          "$FLOW" stop "$OUT_DIR" 1> /dev/null 2>&1
          cleanup
          exit 1
        }
        trap 'cleanup' RETURN
        trap 'force_cleanup' EXIT

        # Some tests mutate the source directory, so make a copy first and run
        # from there. the . in "$dir/." copies the entire directory, including
        # hidden files like .flowconfig.
        cp -R "$dir/." "$OUT_DIR"
        cp "$dir/../assert.sh" "$OUT_PARENT_DIR" 2>/dev/null || :
        cp "$dir/../fs.sh" "$OUT_PARENT_DIR" 2>/dev/null || :
        mv "$OUT_DIR/$exp_file" "$OUT_PARENT_DIR"

        export FLOW_TEMP_DIR="$OUT_PARENT_DIR"

        out_file="$name.out"
        log_file="$name.log"
        monitor_log_file="$name.monitor_log"
        err_file="$name.err"
        diff_file="$name.diff"
        abs_out_file="$OUT_PARENT_DIR/$name.out"
        abs_log_file="$OUT_PARENT_DIR/$name.log"
        abs_monitor_log_file="$OUT_PARENT_DIR/$name.monitor_log"
        abs_err_file="$OUT_PARENT_DIR/$name.err"
        abs_diff_file="$OUT_PARENT_DIR/$name.diff"
        return_status=$RUNTEST_SUCCESS

        export FLOW_LOG_FILE="$abs_log_file"
        export FLOW_MONITOR_LOG_FILE="$abs_monitor_log_file"

        # run the tests from inside $OUT_DIR
        pushd "$OUT_DIR" >/dev/null

        # get config flags.
        # for now this is kind of ad-hoc:
        #
        # 1. The default flow command is check with the --all flag. This flag
        # can be overridden here with the line "all: false". Anything besides
        # "false" is ignored, and the setting itself is ignored if a command
        # besides check is run.
        #
        # 2. The default flow command can be overridden here by supplying the
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
        flowlib=" --no-flowlib"
        shell=""
        cmd="check"
        stdin=""
        stderr_dest="$abs_err_file"
        ignore_stderr=true
        cwd=""
        start_args=""
        file_watcher="none"
        if [ -e ".testconfig" ]
        then
            # all
            if [ "$(awk '$1=="all:"{print $2}' .testconfig)" == "false" ]
            then
                all=""
            fi
            # cwd (current directory)
            cwd="$(awk '$1=="cwd:"{print $2}' .testconfig)"
            # ignore_stderr
            if [ "$(awk '$1=="ignore_stderr:"{print $2}' .testconfig)" == "false" ]
            then
                ignore_stderr=false
                stderr_dest="$abs_out_file"
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
            # file_watcher
            config_fw="$(awk '$1=="file_watcher:"{print $2}' .testconfig)"
            if [ "$config_fw" != "" ]
            then
              file_watcher="$config_fw"
            fi
            # start_args
            config_start_args="$(awk '$1=="start_args:"{$1="";print}' .testconfig)"
            if [ "$config_start_args" != "" ]
            then
                start_args="$config_start_args"
            fi
            # cmd
            config_cmd="$(awk '$1=="cmd:"{$1="";print}' .testconfig)"
            if [ "$config_cmd" != "" ]
            then
                cmd="$config_cmd"
            fi
        fi

        if [ "$cwd" != "" ]; then
            pushd "$cwd" >/dev/null
        fi

        # if .flowconfig sets no_flowlib, don't pass the cli flag
        if grep -q "no_flowlib" .flowconfig; then
            flowlib=""
        fi

        # run test
        if [ "$cmd" == "check" ]
        then
            # default command is check with configurable --all and --no-flowlib
            "$FLOW" check . \
              $all $flowlib --strip-root --show-all-errors \
               1> "$abs_out_file" 2> "$stderr_dest"
            st=$?
            if [ $ignore_stderr = true ] && [ $st -ne 0 ] && [ $st -ne 2 ]; then
              return_status=$RUNTEST_ERROR
            fi
        else
            # otherwise, run specified flow command, then kill the server

            # start server and wait
            "$FLOW" start . \
              $all $flowlib --wait \
              --file-watcher "$file_watcher" \
              --log-file "$abs_log_file" \
              --monitor-log-file "$abs_monitor_log_file" \
              $start_args \
              > /dev/null 2>&1
            code=$?
            if [ $code -ne 0 ]; then
              # flow failed to start
              printf "flow start exited code %s\n" "$code" > "$abs_out_file"
              return_status=$RUNTEST_ERROR
            elif [ "$shell" != "" ]; then
              # run test script
              /bin/bash -e "$shell" "$FLOW" 1> "$abs_out_file" 2> "$stderr_dest"
              code=$?
              if [ $code -ne 0 ]; then
                printf "%s exited code %s\n" "$shell" "$code" >> "$abs_out_file"
                return_status=$RUNTEST_ERROR
              fi
            else
            # If there's stdin, then direct that in
            # cmd should NOT be double quoted...it may contain many commands
            # and we do want word splitting
                if [ "$stdin" != "" ]
                then
                    cmd="$FLOW $cmd < $stdin 1> $abs_out_file 2> $stderr_dest"
                else
                    cmd="$FLOW $cmd 1> $abs_out_file 2> $stderr_dest"
                fi
                eval "$cmd"
            fi
            # stop server
            "$FLOW" stop . 1> /dev/null 2>&1
        fi

        if [ "$cwd" != "" ]; then
            popd >/dev/null
        fi

        # leave $OUT_DIR
        popd >/dev/null

        if [ $return_status -eq $RUNTEST_SUCCESS ]; then
          pushd "$OUT_PARENT_DIR" >/dev/null
          diff -u --strip-trailing-cr "$exp_file" "$out_file" > "$diff_file"
          popd >/dev/null
        fi

        if [ $return_status -ne $RUNTEST_SUCCESS ]; then
            [ -s "$abs_out_file" ] && mv "$abs_out_file" "$dir"
            [ -s "$abs_log_file" ] && mv "$abs_log_file" "$dir"
            [ -s "$abs_monitor_log_file" ] && mv "$abs_monitor_log_file" "$dir"
            [ -s "$abs_err_file" ] && mv "$abs_err_file" "$dir"
            return $return_status
        elif [ -s "$abs_diff_file" ]; then
            mv "$abs_out_file" "$dir"
            [ -s "$abs_log_file" ] && mv "$abs_log_file" "$dir"
            [ -s "$abs_monitor_log_file" ] && mv "$abs_monitor_log_file" "$dir"
            [ -s "$abs_err_file" ] && mv "$abs_err_file" "$dir"
            mv "$abs_diff_file" "$dir"
            return $RUNTEST_FAILURE
        else
            rm -f "$dir/$out_file"
            rm -f "$dir/$log_file"
            rm -f "$dir/$monitor_log_file"
            rm -f "$dir/$err_file"
            rm -f "$dir/$diff_file"
            return $RUNTEST_SUCCESS
        fi
    else
        return $RUNTEST_SKIP
    fi
}


num_to_run_in_parallel=${FLOW_RUNTESTS_PARALLELISM-16}
if [[ "$quiet" -eq 0 ]]; then
  printf "Running up to %d test(s) in parallel\n" "$num_to_run_in_parallel"
fi

# Index N of pids should correspond to the test at index N of dirs
dirs=(tests/*/)
pids=()
next_test_index=0
next_test_to_reap=0

function term_tests() {
  if (( next_test_to_reap < ${#dirs[@]} )); then
    local to_kill=(${pids[@]:$next_test_to_reap})
    #printf "Aborting, killing %s\n" "${to_kill[*]}" >&2;
    printf "\nAborting, killing running tests...\n" >&2
    kill -TERM "${to_kill[@]}" 2>/dev/null
    wait "${to_kill[@]}" 2>/dev/null
    exit 1
  fi
}
trap term_tests TERM SIGINT

# Starts running a test in the background. If there are no more tests then it
# does nothing
start_test() {
    if (( next_test_index < ${#dirs[@]} )); then
        test_dir="${dirs[next_test_index]}"
        runtest "$test_dir" &
        pids[$next_test_index]=$!
    fi
    ((next_test_index++))
}

if [[ "$list_tests" -eq 1 ]]; then
  for dir in "${dirs[@]}"; do
    dir=${dir%*/}
    name=${dir##*/}
    if [[ -z $filter || $name =~ $filter ]]; then
      echo "$name"
    fi
  done
  exit 0
fi

# Kick off a bunch of test runs
for ignore_me in $(seq $num_to_run_in_parallel); do
  start_test
done

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
        print_skip "$testname" "$verbose" ;;
      $RUNTEST_MISSING_FILES )
        (( errored++ ))
        print_error "$testname"
        name=${testname%*/}
        name=${name##*/}
        printf "Missing %s.exp file or .flowconfig file\n" "$name" ;;
      $RUNTEST_ERROR )
        (( errored++ ))
        print_error "$testname" ;;
    esac

    ((next_test_to_reap++))

    # Start up the next test
    start_test
done

if [ $failed -eq 0 ]; then
  FAILED_COLOR="$COLOR_DEFAULT_BOLD"
else
  FAILED_COLOR="$COLOR_WHITE_ON_RED_BOLD"
fi
if [ $errored -eq 0 ]; then
  ERRORED_COLOR="$COLOR_DEFAULT_BOLD"
else
  ERRORED_COLOR="$COLOR_WHITE_ON_RED_BOLD"
fi

if [[ "$quiet" -eq 0 ]]; then
  echo
  printf "%bPassed: %d, %bFailed: %d%b, Skipped: %d, %bErrored: %d%b\n" \
    "$COLOR_DEFAULT_BOLD" "$passed" \
    "$FAILED_COLOR" "$failed" \
    "$COLOR_DEFAULT_BOLD" "$skipped" \
    "$ERRORED_COLOR" "$errored" \
    "$COLOR_RESET"
fi

exit $((failed + errored))
