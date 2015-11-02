FLOW=$1
$FLOW stop 2> /dev/null > /dev/null
for i in $(seq 1 20); do 
  $FLOW status 2> /dev/null > /dev/null &
done 
$FLOW status
