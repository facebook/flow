#!/bin/sh

FLOW=$1
mkdir tmp

printf "\nInitial status:\n"
$FLOW status --no-auto-start --old-output-format .

printf "\nDelete non-@flow file foo.js:\n"
mv foo.js tmp
$FLOW force-recheck --no-auto-start foo.js
$FLOW status --no-auto-start . --old-output-format

printf "\nRestore non-@flow file foo.js:\n"
mv tmp/foo.js .
# NOTE: force-rechecking foo.js defeats the purpose of this particular test
# $FLOW force-recheck --no-auto-start foo.js
$FLOW status --no-auto-start --old-output-format .

rmdir tmp
printf "\nDone!\n"
