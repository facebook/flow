FLOW=$1

$FLOW find-module --strip-root --json ./req test.js
$FLOW get-def --strip-root --json test.js 2 2
$FLOW get-def --strip-root --json test.js 1 10
$FLOW get-def --strip-root --json test.js 4 18
