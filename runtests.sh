#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

THIS_DIR=$(cd -P "$(dirname "$(readlink "${BASH_SOURCE[0]}" || echo "${BASH_SOURCE[0]}")")" && pwd)
export THIS_DIR

show_help() {
  printf "Usage: runtests.sh [-ghlqrv] [-d DIR] [-t TEST] [-b] FLOW_BINARY [[-f] TEST_FILTER]\n\n"
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
  echo "    -z"
  echo "        test new signatures"
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
old_signatures=0
verbose=0
quiet=0
relative="$THIS_DIR"
list_tests=0
export saved_state filter old_signatures
while getopts "b:d:f:lqrszt:vh?" opt; do
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
  z)
    old_signatures=1
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
  elif [[ "$old_signatures" -eq 1 ]]; then
    specific_test=$(echo $specific_test | sed 's/\(.*\)-old-signatures$/\1/')
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

export FLOW VERSION

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

# shellcheck source=fbcode/flow/scripts/lib/runtests-common.sh
. "$THIS_DIR/scripts/lib/runtests-common.sh"

if [ -z "$FLOW_MAX_WORKERS" ]; then
  export FLOW_MAX_WORKERS=2
fi

num_to_run_in_parallel=${FLOW_RUNTESTS_PARALLELISM-16}
if [[ "$quiet" -eq 0 ]]; then
  printf "Running up to %d test(s) in parallel\n" "$num_to_run_in_parallel"
fi

# Index N of results should correspond to the test at index N of dirs
dirs=(tests/*/)
results=()

if [[ "$list_tests" -eq 1 ]]; then
  for dir in "${dirs[@]}"; do
    dir=${dir%*/}
    name=${dir##*/}

    if [[ -z $filter || $name =~ $filter ]]; then

      if [[ "$saved_state" -eq 1 ]]; then
        echo "$name-saved-state"
      elif [[ "$old_signatures" -eq 1 ]]; then
        echo "$name-old-signatures"
      else
        echo "$name"
      fi
    fi
  done
  exit 0
fi

# Run all tests, num_to_run_in_parallel at a time
parallel_run_tests() {
    (
        i=0
        for dir in "${dirs[@]}"; do
            echo "$(( i++ )):$dir"
        done
    ) | xargs -P"$num_to_run_in_parallel" -n1 \
          "$THIS_DIR/scripts/run-one-test"
}

# Kick off all tests, reporting results over a pipe to our fd 3
exec 3< <(parallel_run_tests)

# Print a test's result, given its index in dirs and results
print_result() {
    local test_to_print=$1
    code=${results[$test_to_print]}
    testname=${dirs[$test_to_print]}
    case $code in
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
      $RUNTEST_ERROR | '')
        # '' means the parallel runner stopped before giving a result
        (( errored++ ))
        print_error "$testname" ;;
    esac
}

# Collect the results in the order the tests finish
next_test_to_print=0
print_run "${dirs[$next_test_to_print]}"
while read -ru 3 index code; do
    results[$index]=$code

    # Print the results in order in a pretty way
    while [ -n "${results[$next_test_to_print]}" ]; do
        print_result "$next_test_to_print"
        (( next_test_to_print++ ))
    done

    if (( next_test_to_print < ${#dirs[@]} )); then
        print_run "${dirs[$next_test_to_print]}"
    fi
done

while (( next_test_to_print < ${#dirs[@]} )); do
    # If the parallel runner stopped early, print the remaining tests
    # (as errors, except any where we already have a result)
    print_result "$next_test_to_print"
    (( next_test_to_print++ ))
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
