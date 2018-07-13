#!/bin/bash

# All scripts that source this file will exit if any command fails. Use the
# assert functions below to expect specific exit codes.
set -e

assert_exit_on_line() {
  _assert_exit__line=$1; shift
  _assert_exit__ret=0
  _assert_exit__code=$1; shift
  "$@" ||  _assert_exit__ret=$?
  if [ "$_assert_exit__ret" -eq "$_assert_exit__code" ]; then
    return 0
  else
    echo \
      "\`$(basename "$1") ${*:2}\` expected to exit code $_assert_exit__code" \
      "but got $_assert_exit__ret (line $_assert_exit__line)"
    return 1
  fi
}

export EXIT_OK=0
export EXIT_ERRS=2
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
