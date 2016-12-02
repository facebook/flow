#!/bin/sh

FLOW=$1
mkdir tmp

printf "\nInitial status:\n"
$FLOW status --no-auto-start .

printf "\nDelete c.js:\n"
mv c.js tmp
$FLOW force-recheck --no-auto-start c.js
$FLOW status --no-auto-start .

printf "\nDelete b.js:\n"
mv b.js tmp
$FLOW force-recheck --no-auto-start b.js
$FLOW status --no-auto-start .

printf "\nRestore b.js:\n"
mv tmp/b.js .
$FLOW force-recheck --no-auto-start b.js
$FLOW status --no-auto-start .

printf "\nRestore c.js:\n"
mv tmp/c.js .
$FLOW force-recheck --no-auto-start c.js
$FLOW status --no-auto-start .

printf "\nDelete d.json:\n"
mv d.json tmp
$FLOW force-recheck --no-auto-start d.json
$FLOW status --no-auto-start .

mv tmp/d.json .

printf "\nDelete unchecked.js:\n"
mv unchecked.js tmp
$FLOW force-recheck --no-auto-start unchecked.js
$FLOW status --no-auto-start .

printf "\nRestore unchecked.js:\n"
mv tmp/unchecked.js .
$FLOW force-recheck --no-auto-start unchecked.js
$FLOW status --no-auto-start .

printf "\nDelete dupe2.js:\n"
mv dupe2.js tmp
$FLOW force-recheck --no-auto-start dupe2.js
$FLOW status --no-auto-start .

printf "\nRestore dupe2.js:\n"
mv tmp/dupe2.js .
$FLOW force-recheck --no-auto-start dupe2.js
$FLOW status --no-auto-start .

rmdir tmp
printf "\nDone!\n"
