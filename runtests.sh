#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

THIS_DIR=$(cd -P "$(dirname "$(readlink "${BASH_SOURCE[0]}" || echo "${BASH_SOURCE[0]}")")" && pwd)

# Use the assert functions below to expect specific exit codes.

assert_exit_on_line() {
  # Run all this in a subshell, so we can set -e without affecting the caller
  (
    set -e
    _assert_exit__line=$1; shift
    _assert_exit__ret=0
    _assert_exit__code=$1; shift
    "$@" ||  _assert_exit__ret=$?
    eval "$SAVED_OPTION" # Undo the `set -e` if necessary
    if [ "$_assert_exit__ret" -eq "$_assert_exit__code" ]; then
      return 0
    else
      echo \
        "\`$(basename "$1") ${*:2}\` expected to exit code $_assert_exit__code" \
        "but got $_assert_exit__ret (line $_assert_exit__line)"
      return 1
    fi
  )
  return $?
}

export EXIT_OK=0
export EXIT_ERRS=2
export EXIT_INVALID_FLOWCONFIG=8
export EXIT_COULD_NOT_FIND_FLOWCONFIG=12
export EXIT_USAGE=64

assert_exit() {
  assert_exit_on_line "${BASH_LINENO[0]}" "$@"
}
assert_ok() {
  assert_exit_on_line "${BASH_LINENO[0]}" "$EXIT_OK" "$@"
}
assert_errors() {
  assert_exit_on_line "${BASH_LINENO[0]}" "$EXIT_ERRS" "$@"
}

# Query utilities

query_at_pos() {
  local query=$1
  local file=$2
  local line=$3
  local col=$4
  shift 4
  local flags=("$@")

  printf "%s:%s:%s\n" "$file" "$line" "$col"
  echo "Flags:" "${flags[@]}"
  # shellcheck disable=SC2094
  assert_ok "$FLOW" "$query" "$file" "$line" "$col" --strip-root "${flags[@]}" < "$file"
  printf "\n"
}

# Utility to create queries that involve specific locations (line, column)
#
# This function performs 'query' on all locations (line, col) in 'file' that
# have a ^ in comments at location (line+1, col).
#
# E.g.
#   type A = number;
#   //   ^
#
# You can also pass in flags after the ^:
# //   ^ --expand-type-aliases
#
# Invoke this passing the query name, the file, and any additional flags to be
# applied to every query in the file, e.g.
#
#   query_in_file "type-at-pos" "file.js" --pretty --json
#
queries_in_file() {
  local query=$1
  local file=$2
  shift 2
  local arg_flags_array=("$@")

  awk '/^\/\/.*\^/{ print NR }' "$file" | while read -r line; do
    local linep="$line""p"
    # Compute the column of the query
    local col
    col=$(
      sed -n "$linep" "$file" | \
      awk -F'^' '{ print $1}' | \
      wc -c
    )
    col="$((col))" # trim wc's whitespaces
    # Read line flags into an array (space separation)
    local line_flags_array=()
    IFS=" " read -r -a line_flags_array <<< "$(
      sed -n "$linep" "$file" | \
      awk -F'^' '{ print $2 }'
    )"
    if [ -n "$col" ]; then
      # Get line above
      ((line--))
      # Aggregate flags
      local all_flags=("${arg_flags_array[@]}" "${line_flags_array[@]}")
      query_at_pos "$query" "$file" "$line" "$col" "${all_flags[@]}"
    fi
  done
}

show_skipping_stats_classic() {
  printf "\\n========Skipping stats========\\n"
  grep -o "Merge skipped [0-9]\+ of [0-9]\+ modules" $1 | tail -n 1
}

show_skipping_stats_types_first() {
  printf "\\n========Skipping stats========\\n"
  grep -o "Merge skipped [0-9]\+ of [0-9]\+ modules" $1 | tail -n 1
  grep -o "Check will skip [0-9]\+ of [0-9]\+ files" $1 | tail -n 1
}

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
  echo "    -s"
  echo "        test saved state"
  echo "    -v"
  echo "        verbose output (shows skipped tests)"
  echo "    -h"
  echo "        display this help and exit"
}

export IN_FLOW_TEST=1
export FLOW_LOG_LEVEL=debug
export FLOW_NODE_BINARY=${FLOW_NODE_BINARY:-${NODE_BINARY:-$(which node)}}

OPTIND=1
record=0
saved_state=0
verbose=0
quiet=0
relative="$THIS_DIR"
list_tests=0
while getopts "b:d:f:lqrst:vh?" opt; do
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
    specific_test="$OPTARG"
    ;;
  q)
    quiet=1
    ;;
  r)
    record=1
    ;;
  s)
    saved_state=1
    printf "Testing saved state by running all tests using saved state\\n"
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

if [ -n "$specific_test" ]; then
  if [[ "$saved_state" -eq 1 ]]; then
    specific_test=$(echo $specific_test | sed 's/\(.*\)-saved-state$/\1/')
  fi

  filter="^$specific_test$"
fi

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

VERSION=$("$FLOW" version --semver)

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
      # Copy .out to .exp, replacing the current version, if present, with
      # <VERSION>, so that the .exp doesn't have to be updated on each release.
      sed 's/'"${VERSION//./\\.}"'/<VERSION>/g' "${dir}${name}.out" > "${dir}${name}.exp"
      rm "${dir}${name}.out"
      rm -f "$err_file"
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
               [ "$name" = "callable" ]
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
        # 1. The default flow command can be overridden here by supplying the
        # entire command on a line that begins with "cmd:". (Note: for writing
        # incremental tests, use the option below). A line beginning with
        # "stdin:" can additionally supply arguments to pass to the command via
        # standard input. We start the server before running the command and
        # stop the server after we get a result.
        #
        # 2. Integration tests that interleave flow commands with file system
        # commands (for testing incremental checks, e.g.) are written by
        # supplying the name of a shell script on a line that begins with
        # "shell:". The shell script is passed the location of the flow binary
        # as argument ($1). We start the server before running the script and
        # stop the server after the script exits.
        #
        auto_start=true
        flowlib=" --no-flowlib"
        shell=""
        cmd="check"
        stdin=""
        stderr_dest="$abs_err_file"
        ignore_stderr=true
        cwd=""
        start_args=""
        file_watcher="none"
        wait_for_recheck="true"
        types_first_flag="--types-first"
        if [ -e ".testconfig" ]
        then
            # auto_start
            if [ "$(awk '$1=="auto_start:"{print $2}' .testconfig)" == "false" ]
            then
                auto_start=false
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
            config_cmd="${config_cmd## }" # trim leading space
            if [ "$config_cmd" != "" ]
            then
                cmd="$config_cmd"
            fi

            # classic_only
            if [ "$(awk '$1=="classic_only:"{print $2}' .testconfig)" == "true" ]
            then
                types_first_flag=""
            fi

            if [[ "$saved_state" -eq 1 ]] && \
              [ "$(awk '$1=="skip_saved_state:"{print $2}' .testconfig)" == "true" ]
            then
                return $RUNTEST_SKIP
            fi

            if [[ "$saved_state" -eq 0 ]] && \
              [ "$(awk '$1=="saved_state_only:"{print $2}' .testconfig)" == "true" ]
            then
                return $RUNTEST_SKIP
            fi

            # wait_for_recheck
            if [ "$(awk '$1=="wait_for_recheck:"{print $2}' .testconfig)" == "false" ]
            then
                wait_for_recheck="false"
            fi
        fi

        if [ "$cwd" != "" ]; then
            pushd "$cwd" >/dev/null
        fi

        # if .flowconfig sets no_flowlib, don't pass the cli flag
        if [ -f .flowconfig ] && grep -q "no_flowlib" .flowconfig; then
            flowlib=""
        fi

        # Helper function to generate saved state. If anything goes wrong, it
        # will fail
        create_saved_state () {
          local root="$1"
          local flowconfig_name="$2"
          (
            set -e
            # start lazy server and wait
            "$FLOW" start "$root" \
              $flowlib --wait \
              $types_first_flag \
              --wait-for-recheck "$wait_for_recheck" \
              --lazy \
              --file-watcher "$file_watcher" \
              --flowconfig-name "$flowconfig_name" \
              --log-file "$abs_log_file" \
              --monitor-log-file "$abs_monitor_log_file"

            local SAVED_STATE_FILENAME="$root/.flow.saved_state"
            local CHANGES_FILENAME="$root/.flow.saved_state_file_changes"
            assert_ok "$FLOW" save-state \
              --root "$root" \
              --out "$SAVED_STATE_FILENAME" \
              --flowconfig-name "$flowconfig_name"
            assert_ok "$FLOW" stop --flowconfig-name "$flowconfig_name" "$root"
            touch "$CHANGES_FILENAME"
          )  > /dev/null 2>&1
          local RET=$?
          return $RET
        }

        # run test
        if [ "$cmd" == "check" ]
        then
            if [[ "$saved_state" -eq 1 ]]; then
              if create_saved_state . ".flowconfig"; then
                # default command is check with configurable --no-flowlib
                "$FLOW" check . \
                  $flowlib --strip-root --show-all-errors \
                  $types_first_flag \
                  --saved-state-fetcher "local" \
                  --saved-state-no-fallback \
                   1>> "$abs_out_file" 2>> "$stderr_dest"
                st=$?
              else
                printf "Failed to generate saved state\\n" >> "$stderr_dest"
                return_status=$RUNTEST_ERROR
              fi
            else
              # default command is check with configurable --no-flowlib
              "$FLOW" check . \
                $flowlib --strip-root --show-all-errors \
                $types_first_flag \
                 1>> "$abs_out_file" 2>> "$stderr_dest"
              st=$?
            fi
            if [ $ignore_stderr = true ] && [ -n "$st" ] && [ $st -ne 0 ] && [ $st -ne 2 ]; then
              printf "flow check return code: %d\\n" "$st" >> "$stderr_dest"
              return_status=$RUNTEST_ERROR
            fi
        elif [ "$(echo "$cmd" | awk '{print $1}')" == "codemod" ]
        then
          subcmd=$(echo "$cmd" | awk '{print $2}')

          # parse flags after 'cmd: codemod subcommand'
          config_cmd_args="$(echo "$cmd" | awk '{$1="";$2="";print}')"
          cmd_args=("$config_cmd_args")

          # shellcheck disable=SC2086
          "$FLOW" "codemod" "$subcmd" \
              $flowlib \
              ${cmd_args[*]} \
              $types_first_flag \
              --strip-root \
              --quiet \
              . \
              1>> "$abs_out_file" \
              2>> "$stderr_dest"
          st=$?

          if [ $ignore_stderr = true ] && [ -n "$st" ] && [ $st -ne 0 ] && [ $st -ne 2 ]; then
            printf "flow codemod return code: %d\\n" "$st" >> "$stderr_dest"
            return_status=$RUNTEST_ERROR
          fi
        else
            # otherwise, run specified flow command, then kill the server

            # start_flow_unsafe ROOT [OPTIONS]...
            start_flow_unsafe () {
              local root=$1; shift

              if [ ! -d "$root" ]; then
                printf "Invalid root directory '%s'\\n" "$root" >&2
                return 1
              fi

              if [[ "$saved_state" -eq 1 ]]; then
                local flowconfig_name=".flowconfig"
                for ((i=1; i<=$#; i++)); do
                  opt="${!i}"
                  if [ "$opt" = "--flowconfig-name" ]
                  then
                    ((i++))
                    flowconfig_name=${!i}
                  fi
                done

                if create_saved_state "$root" "$flowconfig_name"
                then
                  PATH="$THIS_DIR/scripts/tests_bin:$PATH" \
                  "$FLOW" start "$root" \
                    $flowlib --wait \
                    $types_first_flag \
                    --wait-for-recheck "$wait_for_recheck" \
                    --saved-state-fetcher "local" \
                    --saved-state-no-fallback \
                    --file-watcher "$file_watcher" \
                    --log-file "$abs_log_file" \
                    --monitor-log-file "$abs_monitor_log_file" \
                    "$@"
                  return $?
                else
                    printf "Failed to generate saved state\\n" >&2
                    return 1
                fi
              else
                # start server and wait
                PATH="$THIS_DIR/scripts/tests_bin:$PATH" \
                "$FLOW" start "$root" \
                  $flowlib --wait --wait-for-recheck "$wait_for_recheck" \
                  $types_first_flag \
                  --file-watcher "$file_watcher" \
                  --log-file "$abs_log_file" \
                  --monitor-log-file "$abs_monitor_log_file" \
                  "$@"
                return $?
              fi
            }

            # start_flow_unsafe ROOT [OPTIONS]...
            start_flow () {
              assert_ok start_flow_unsafe "$@"
            }

            if [ $auto_start = true ]; then
              start_flow_unsafe . $start_args > /dev/null 2>> "$abs_err_file"
              code=$?
              if [ $code -ne 0 ]; then
                # flow failed to start
                printf "flow start exited code %s\\n" "$code" >> "$abs_out_file"
                return_status=$RUNTEST_ERROR
              fi
            fi

            if [ $return_status -ne $RUNTEST_ERROR ]; then
              if [ "$shell" != "" ]; then
                # run test script in subshell so it inherits functions
                (
                  set -e # The script should probably use this option
                  export PATH="$THIS_DIR/scripts/tests_bin:$PATH"
                  source "$shell" "$FLOW"
                ) 1> "$abs_out_file" 2> "$stderr_dest"
                code=$?
                if [ $code -ne 0 ]; then
                  printf "%s exited code %s\\n" "$shell" "$code" >> "$abs_out_file"
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

              # stop server, even if we didn't start it
              "$FLOW" stop . 1> /dev/null 2>&1
            fi
        fi

        if [ "$cwd" != "" ]; then
            popd >/dev/null
        fi

        # leave $OUT_DIR
        popd >/dev/null

        if [ $return_status -eq $RUNTEST_SUCCESS ]; then
          pushd "$OUT_PARENT_DIR" >/dev/null
          # When diffing the .exp against the .out, replace <VERSION> in the
          # .exp with the actual version, so the diff shows which version we
          # were expecting, but the .exp doesn't need to be updated for each
          # release.
          diff -u --strip-trailing-cr \
            --label "$exp_file" --label "$out_file" \
            <(awk '{gsub(/<VERSION>/, "'"$VERSION"'"); print $0}' "$exp_file") \
            "$out_file" \
            > "$diff_file" 2>&1
          popd >/dev/null
        fi

        if [ $return_status -ne $RUNTEST_SUCCESS ]; then
            [ -s "$abs_out_file" ] && mv "$abs_out_file" "$dir" || rm -f "$dir/$out_file"
            [ -s "$abs_log_file" ] && mv "$abs_log_file" "$dir" || rm -f "$dir/$log_file"
            [ -s "$abs_monitor_log_file" ] && mv "$abs_monitor_log_file" "$dir" || rm -f "$dir/$monitor_log_file"
            [ -s "$abs_err_file" ] && mv "$abs_err_file" "$dir" || rm -f "$dir/$err_file"
            return $return_status
        elif [ -s "$abs_diff_file" ]; then
            mv "$abs_out_file" "$dir"
            [ -s "$abs_log_file" ] && mv "$abs_log_file" "$dir" || rm -f "$dir/$log_file"
            [ -s "$abs_monitor_log_file" ] && mv "$abs_monitor_log_file" "$dir" || rm -f "$dir/$monitor_log_file"
            [ -s "$abs_err_file" ] && mv "$abs_err_file" "$dir" || rm -f "$dir/$err_file"
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

if [ -z "$FLOW_MAX_WORKERS" ]; then
  export FLOW_MAX_WORKERS=2
fi

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

      if [[ "$saved_state" -eq 1 ]]; then
        echo "$name-saved-state"
      else
        echo "$name"
      fi
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
