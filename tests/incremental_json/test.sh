
FLOW=$1

printf "\nInitial status:\n"
$FLOW status --no-auto-start .

printf "\nCreate data.json:\n"
cp data.json1 data.json
$FLOW force-recheck --no-auto-start test.js
$FLOW status --no-auto-start .

printf "\nModify data.json:\n"
cp data.json2 data.json
$FLOW force-recheck --no-auto-start test.js
$FLOW status --no-auto-start .

printf "\nDelete data.json:\n"
rm data.json
$FLOW force-recheck --no-auto-start test.js
$FLOW status --no-auto-start .

printf "\nDone!\n"
