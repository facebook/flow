#!/bin/sh

FLOW=$1

printf "Without --include-suppressed:\n"
$FLOW check
printf "With --include-suppressed:\n"
$FLOW check --include-suppressed
