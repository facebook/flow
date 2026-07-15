#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

write_flowconfig_with_lints() {
  local root=$1
  mkdir -p "$root"
  cat > "$root/.flowconfig" <<'EOF'
[options]
all=true
include_warnings=true

[lints]
all=error
EOF
}

write_flowconfig() {
  local root=$1
  mkdir -p "$root"
  cat > "$root/.flowconfig" <<'EOF'
[options]
all=true
include_warnings=true
EOF
}

print_file() {
  local file=$1
  printf "%s\n" ">>> $file"
  sed -n '1,120p' "$file"
}

flow_bin_arg() {
  if command -v cygpath > /dev/null 2>&1; then
    cygpath -m "$FLOW"
  else
    printf "%s\n" "$FLOW"
  fi
}

FLOW_BIN=$(flow_bin_arg)

printf "=== remove-comments keeps remaining flowlint suppressions ===\n"
write_flowconfig_with_lints remove_partial
cat > remove_partial/test.js <<'EOF'
function f(x: ?number) {
  // flowlint-next-line sketchy-null-string:off, sketchy-null-number:off
  if (x) {}
}
EOF
assert_ok "$FLOW" dev-tools remove-comments --bin "$FLOW_BIN" --check full-check remove_partial
print_file remove_partial/test.js

printf "\n=== remove-comments removes empty flowlint suppressions ===\n"
write_flowconfig_with_lints remove_empty
cat > remove_empty/test.js <<'EOF'
function f(x: number) {
  // flowlint-next-line sketchy-null-string:off
  if (x) {}
}
EOF
assert_ok "$FLOW" dev-tools remove-comments --bin "$FLOW_BIN" --check full-check remove_empty
print_file remove_empty/test.js

printf "\n=== update-suppressions preserves eslint suppressions and adds missing ones ===\n"
write_flowconfig update_suppressions
cat > update_suppressions/test.js <<'EOF'
// $FlowFixMe[incompatible-type] eslint-disable-next-line no-fallthrough
const ok: number = 1;

function takesNumber(x: number) {}
takesNumber("nope");
EOF
assert_ok "$FLOW" dev-tools update-suppressions --bin "$FLOW_BIN" --check full-check update_suppressions
print_file update_suppressions/test.js

printf "\n=== update-suppressions updates site annotations across roots ===\n"
mkdir -p update_sites/foo update_sites/bar
cat > update_sites/foo/.flowconfig <<'EOF'
[include]
../shared.js

[options]
all=true
include_warnings=true

[lints]
all=off
EOF
cat > update_sites/bar/.flowconfig <<'EOF'
[include]
../shared.js

[options]
all=true
include_warnings=true

[lints]
all=error
EOF
cat > update_sites/shared.js <<'EOF'
function f(x: ?number) {
  // $FlowFixMe[sketchy-null-number](site=bar,foo)
  if (x) {}
}
EOF
assert_ok "$FLOW" dev-tools update-suppressions --bin "$FLOW_BIN" --check full-check --sites foo,bar update_sites/foo update_sites/bar
print_file update_sites/shared.js

printf "\n=== add-comments adds suppressions for errors ===\n"
write_flowconfig add_comments
cat > add_comments/test.js <<'EOF'
function takesNumber(x: number) {}
takesNumber("nope");
EOF
assert_ok "$FLOW" dev-tools add-comments --bin "$FLOW_BIN" --check full-check --comment e2e add_comments
print_file add_comments/test.js
