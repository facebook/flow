#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Regenerate every codegen artifact produced by the flow_parser_wasm:codegen
# binary. The source of truth for generated artifacts is the SCHEMA in
#   fbcode/flow/rust_port/crates/flow_parser_wasm/src/node_kinds.rs
# Flow owns flow-estree/src/types.js; this script validates it against SCHEMA.
#
# Run from anywhere; this script locates the fbsource root before invoking
# buck. Override FLOW_ESTREE_TYPES_JS to validate another Flow-owned types file.
#
# Each artifact is written to a sibling .tmp file first; on codegen success
# the temp file is atomically renamed into place, on failure the script exits
# without touching the live artifact. After every JS/Flow output is written,
# `arc f` is invoked to normalize formatting per repo conventions. The Rust
# dispatch artifact is left unformatted — it is consumed via the
# @fbcode//mode/opt build, which runs rustfmt as part of the build pipeline.
#
# The --rust mode is run LAST: it overwrites serializer_dispatch.rs which is
# `include!`'d by the same crate the codegen binary depends on. Doing it last
# avoids re-linking the codegen binary against a partially-regenerated
# dispatch file inside this script's own run.

set -euo pipefail

# Resolve the fbsource root from this script's location:
#   fbsource/fbcode/flow/rust_port/crates/flow_parser_wasm/regen.sh
# .. levels:               5     4         3      2       1
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
fbsource_root="$(cd "${script_dir}/../../../../.." && pwd)"

if [[ ! -d "${fbsource_root}/fbcode" ]]; then
    echo "error: could not locate fbsource root from ${script_dir}" >&2
    exit 1
fi

cd "${fbsource_root}/fbcode"

export FLOW_ESTREE_TYPES_JS="${FLOW_ESTREE_TYPES_JS:-${fbsource_root}/fbcode/flow/packages/flow-estree/src/types.js}"
export HERMES_ESTREE_SELECTOR_TYPES_JS="${HERMES_ESTREE_SELECTOR_TYPES_JS:-${fbsource_root}/xplat/static_h/tools/hermes-parser/js/hermes-estree/src/generated/HermesESTreeSelectorTypes.js.flow}"

codegen_target="fbcode//flow/rust_port/crates/flow_parser_wasm:codegen"
parser_pkg="flow/packages/flow-parser/oxidized-src"
estree_pkg="flow/packages/flow-estree/src"
dispatch_path="flow/rust_port/crates/flow_parser_wasm/src/serializer_dispatch.rs"

# Ensure cleanup of any leftover .tmp files on exit (success or failure).
cleanup_tmp_files=()
on_exit() {
    local rc=$?
    for f in "${cleanup_tmp_files[@]:-}"; do
        [[ -n "$f" && -f "$f" ]] && rm -f "$f"
    done
    exit "$rc"
}
trap on_exit EXIT

# Write codegen output to a temp file then atomically rename. On codegen
# failure the live artifact is untouched.
run_codegen() {
    local label="$1"
    local out="$2"
    shift 2
    local tmp="${out}.tmp.$$"
    cleanup_tmp_files+=("${tmp}")
    echo "[regen] ${label} -> ${out}" >&2
    if ! buck2 run @fbcode//mode/dev-nosan "${codegen_target}" -- "$@" > "${tmp}"; then
        echo "error: codegen failed for ${label}; ${out} left untouched" >&2
        return 1
    fi
    mv "${tmp}" "${out}"
}

run_arc_f() {
    local out="$1"
    echo "[regen] arc f ${out}" >&2
    arc f "${out}"
}

# 1. JS deserializer (default) — for flow-parser
run_codegen "JS deserializer" "${parser_pkg}/FlowParserNodeDeserializers.js"
run_arc_f "${parser_pkg}/FlowParserNodeDeserializers.js"

# 2. ESTree visitor keys (--estree-visitor-keys) — for flow-parser
run_codegen "ESTree visitor keys" \
    "${parser_pkg}/generated/ESTreeVisitorKeys.js" \
    --estree-visitor-keys
run_arc_f "${parser_pkg}/generated/ESTreeVisitorKeys.js"

# 3. ESTree visitor keys Flow companion (--estree-visitor-keys-flow)
run_codegen "ESTree visitor keys (.flow companion)" \
    "${parser_pkg}/generated/ESTreeVisitorKeys.js.flow" \
    --estree-visitor-keys-flow
run_arc_f "${parser_pkg}/generated/ESTreeVisitorKeys.js.flow"

# 4. Validate the Flow-owned ESTree types against SCHEMA.
echo "[regen] validate ESTree types -> ${estree_pkg}/types.js" >&2
buck2 run @fbcode//mode/dev-nosan "${codegen_target}" -- --check-estree-types

# 5. ESTree predicates (--estree-predicates) — for flow-estree
run_codegen "ESTree predicates" \
    "${estree_pkg}/generated/predicates.js" \
    --estree-predicates
run_arc_f "${estree_pkg}/generated/predicates.js"

# 6. Minimal ESTree selector types used to check flow-estree's sources. The
#    package build generates the complete declaration directly into dist/.
run_codegen "ESTree selector source types" \
    "${estree_pkg}/generated/HermesESTreeSelectorTypes.js.flow" \
    --estree-selectors
run_arc_f "${estree_pkg}/generated/HermesESTreeSelectorTypes.js.flow"

# 7. Rust serializer dispatch (--rust) — for flow_parser_wasm itself.
#    Done last on purpose: serializer_dispatch.rs is `include!`'d by the same
#    crate the codegen binary depends on, so writing it earlier would force
#    every later buck2 invocation to re-link against a freshly-generated file.
run_codegen "Rust serializer dispatch" "${dispatch_path}" --rust

echo "[regen] all 7 codegen modes regenerated successfully" >&2
