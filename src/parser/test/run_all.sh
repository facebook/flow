DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

node $DIR/run_esprima_tests.js
esprima_exit=$?
node $DIR/run_hardcoded_tests.js
hardcoded_exit=$?

if [ $esprima_exit -ne 0 ] || [ $hardcoded_exit -ne 0 ] ; then
  echo
  echo "FAILED: Some tests failed"
  exit 1
fi
