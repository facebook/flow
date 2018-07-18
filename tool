#!/bin/sh

DIR=$( cd -P -- "$(dirname -- "$(command -v -- "$0")")" && pwd -P )
exec "$DIR/packages/flow-dev-tools/bin/tool" "$@"
