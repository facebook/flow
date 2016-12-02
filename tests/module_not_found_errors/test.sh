#!/bin/sh

FLOW=$1
cd src || exit
$FLOW check --strip-root
