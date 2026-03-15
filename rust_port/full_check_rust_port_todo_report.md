# Full-Check Only Port: Remaining Work and Topological Order

## Executive Summary

The Rust port has continued to make strong progress. **364,736 lines of Rust** are implemented across **53 crates**, with only **2 `todo!()` calls** remaining (one debug-only path, one stubbed analysis function). The entire typing subsystem is **100% ported**.

Since the last report, several previously-unported orchestration modules have been completed:
- **`errorCollator.ml`** — fully ported (454 Rust lines)
- **`flowlib.ml`** — fully ported (82 Rust lines + generated content modules)
- **`merge_service.ml` `mk_check`** — fully ported (502 Rust lines total in merge_service.rs)
- **`job_utils.ml`** — fully ported (169 Rust lines)

The remaining work is now focused on two areas:
1. **Init/lib loading** (`init_js.ml` — 156 OCaml lines) — loading built-in type definitions
2. **Check orchestration + wiring** (`Check_files.check_files` in `types_js.ml` + `check_files_for_init` + `init_from_scratch` + CLI wiring)

The core type-checking engine, error collation, job distribution, and flowlib extraction are all complete.

## Current State of the Rust Port

The existing `full_check_from_scratch` function in `flow_services_inference/src/type_service.rs` implements the pipeline up through merge. The check phase and error output remain to be wired in.

| Stage | OCaml | Rust | Status |
|-------|-------|------|--------|
| File discovery | `Files.make_next_files` | `make_next_files` | Done |
| Parsing | `Parsing_service_js.parse_with_defaults` | `parsing_service::parse_with_defaults` | Done |
| Commit modules | `Module_js.commit_modules` | `flow_services_module::commit_modules` | Done |
| Resolve requires | `Module_js.add_parsed_resolved_requires` | `flow_services_module::add_parsed_resolved_requires` | Done |
| Calc dependencies | `Dep_service.calc_dependency_info` | `dep_service::calc_dependency_info` | Done |
| Files to infer | `files_to_infer` / `unfocused_files_to_infer` | `unfocused_files_to_infer` | Done |
| Include deps/dependents | `include_dependencies_and_dependents` | `include_dependencies_and_dependents` | Done |
| Ensure parsed | `ensure_parsed_or_trigger_recheck` | `ensure_parsed_or_trigger_recheck` | Done |
| **Merge** | `merge` (merge_service) | `merge` | **Done** |
| **Check per-file** | `Check_service.mk_check_file` | `check_service::mk_check_file` | **Done** |
| **mk_check wrapper** | `Merge_service.mk_check` | `merge_service::mk_check` | **Done** |
| **Job utils** | `Job_utils.mk_job` / `mk_next` | `job_utils::mk_job` / `mk_next` | **Done** |
| **Check cache** | `Check_cache` | `check_cache` | **Done** |
| **Coverage** | `Coverage.file_coverage` | `flow_services_coverage` | **Done** |
| **Inference utils** | `Inference_utils.*` | `inference_utils` | **Done** |
| **Flowlib extraction** | `Flowlib.extract` | `flow_flowlib::extract` | **Done** |
| **Error collation** | `ErrorCollator.update_collated_errors` | `error_collator::update_collated_errors` | **Done** |
| **Error collation get** | `ErrorCollator.get` | `error_collator::get` | **Done** |
| **Error output** | `format_errors` | `flow_common_errors` | **Done** |
| **Lib loading** | `Init_js.init` | — | **NOT DONE** |
| **Context heaps** | `Context_heaps.add_master/find_master` | — | **NOT DONE** (trivial) |
| **Check orchestration** | `Check_files.check_files` | — | **NOT DONE** |
| **Init from scratch** | `Types_js.init_from_scratch` | — | **NOT DONE** |
| **Check for init** | `Types_js.check_files_for_init` | — | **NOT DONE** |
| **Top-level wiring** | `Server.check_once` | — | **NOT DONE** |

## What Remains: Full-Check Path

### The OCaml full-check path (from `checkCommands.ml`)

```
checkCommands.ml::full_check_main
  └─ Server.check_once
       ├─ create_program_init
       │    ├─ SharedMem.init                                         ✅ PORTED (different model)
       │    ├─ ServerEnvBuild.make_genv                               ⚠️  TRIVIAL (just wraps options)
       │    └─ init (returns program_init closure)
       │         ├─ extract_flowlibs_or_exit → Flowlib.extract        ✅ PORTED
       │         ├─ Types_js.init
       │         │    └─ init_from_scratch
       │         │         ├─ parse (Parsing_service_js)              ✅ PORTED
       │         │         ├─ init_libs → Init_js.init                ❌ NOT PORTED
       │         │         │    └─ load_lib_files
       │         │         │         └─ Merge_js.merge_lib_files      ✅ PORTED
       │         │         ├─ commit_modules                          ✅ PORTED
       │         │         ├─ resolve_requires                        ✅ PORTED
       │         │         ├─ Dep_service.calc_dependency_info        ✅ PORTED
       │         │         └─ ErrorCollator.update_local_collated_errors  ✅ PORTED
       │         └─ Types_js.full_check_for_init
       │              └─ check_files_for_init
       │                   ├─ files_to_infer                          ✅ PORTED
       │                   ├─ include_dependencies_and_dependents     ✅ PORTED
       │                   ├─ merge                                   ✅ PORTED
       │                   ├─ Check_files.check_files                 ❌ NOT PORTED (orchestration)
       │                   │    ├─ Merge_service.mk_check             ✅ PORTED
       │                   │    │    └─ Check_service.mk_check_file   ✅ PORTED
       │                   │    │         └─ Type_inference_js.infer_file  ✅ PORTED
       │                   │    │              └─ statement.ml         ✅ PORTED (18,676 Rust lines)
       │                   │    ├─ Job_utils.mk_job                   ✅ PORTED
       │                   │    └─ Job_utils.mk_next                  ✅ PORTED
       │                   ├─ ErrorCollator.update_collated_errors    ✅ PORTED
       │                   └─ ErrorCollator.update_error_state_timestamps  ✅ PORTED
       ├─ ErrorCollator.get                                           ✅ PORTED
       └─ format_errors                                               ✅ PORTED (flow_common_errors)
```

**Key insight**: All leaf-level functions are now ported. What's missing is the glue code that:
1. Loads lib files (builtins like `string`, `number`, `Array`, `Promise`) via `Init_js.init`
2. Orchestrates per-file checking via `Check_files.check_files` (which calls into the now-ported `mk_check`, `mk_job`, `mk_next`)
3. Wires the check phase into the existing `full_check_from_scratch` pipeline and outputs errors

---

## Modules to Port, in Topological Order

### Tier 1: Leaf Dependencies (small, no further unported deps)

#### 1a. `context_heaps.ml` (38 lines) — master context storage
- **Path**: `flow/src/state/heaps/context/context_heaps.ml`
- **Why needed**: `add_master` stores master context after lib init; `find_master` retrieves it during checking
- **Dependencies**: `Context` (ported), `SharedMem`
- **Note**: In Rust, this is likely just storing in an `Arc<>` or on the `Env` struct. The Rust `server_env.rs` already has `master_cx: MasterContext` on `Env`. The `merge_service::mk_check` already takes `MasterContext` as a parameter — this module may not need a separate port if master_cx is threaded through directly.
- **Complexity**: Simple

### Tier 2: Init/Lib Loading (depends on Tier 1)

#### 2a. `init_js.ml` (156 lines) — lib initialization
- **Path**: `flow/src/services/inference/init_js.ml`
- **Why needed**: `Init_js.init` loads and processes lib definition files (builtins, flowlibs). **Without this, the type checker has no knowledge of built-in types** (`string`, `number`, `Array`, `Promise`, etc.)
- **Key functions**:
  - `load_lib_files` (~70 lines) — parses lib files, calls `Merge_js.merge_lib_files` (already ported), scans for suppressions
  - `error_set_to_filemap` (~12 lines) — helper to convert error sets to per-file maps
  - `init` (~30 lines) — orchestrates lib loading, error collection, stores master context
- **Dependencies**: `Merge_js.merge_lib_files` (ported), `Type_inference_js.scan_for_suppressions` (ported), `Error_suppressions.filter_lints` (ported), `Context_heaps.add_master` (Tier 1a), `Flowlib.extract` (ported)
- **Essential**: YES — without lib initialization, no checking can produce correct results.
- **Complexity**: Medium — uses Lwt (replace with sync), PPX macros (already handled by flowlib)

### Tier 3: Check Orchestration (depends on Tier 2)

#### 3a. `types_js.ml` — `Check_files.check_files` (~120 lines, lines 500-620)
- **Path**: `flow/src/services/inference/types_js.ml` (inner module)
- **Why needed**: Orchestrates the per-file check phase — creates check jobs, distributes work, collects results
- **Dependencies**: `Merge_service.mk_check` (ported), `Job_utils.mk_job/mk_next` (ported), `Context_heaps.find_master` (Tier 1a), check result accumulation helpers
- **Note**: The core logic is: for each file in `to_check`, call `mk_check(file)`, collect errors/warnings/suppressions/coverage. The Rust port can use the same `map_reduce` pattern used for merge, combined with the now-ported `job_utils::mk_next` and `job_utils::mk_job`.
- **Complexity**: Medium — needs `update_check_results` accumulator function and skipping logic for dependents

### Tier 4: Init + Check Wiring (depends on Tier 2 + 3)

#### 4a. `types_js.ml` — `init_from_scratch` (~115 lines, lines 2169-2282)
- **Why needed**: Full init sequence (parse, init_libs, commit, resolve, calc_deps, create `Env`)
- **Note**: Most of this is already implemented in the current `full_check_from_scratch`. The main addition is calling `init_libs` (Init_js.init) after parsing, and calling `ErrorCollator.update_local_collated_errors` after that.
- **Complexity**: Medium — mostly wiring existing ported functions together

#### 4b. `types_js.ml` — `check_files_for_init` (~90 lines, lines 2519-2606)
- **Why needed**: The check phase after merge — calls `Check_files.check_files`, then `ErrorCollator.update_collated_errors`
- **Note**: This is the bridge between merge and error output. It calls `files_to_infer`, `include_dependencies_and_dependents`, `merge`, `Check_files.check_files`, and `ErrorCollator.update_collated_errors` — all of which are now ported individually.
- **Complexity**: Medium — orchestration of already-ported functions

### Tier 5: Top-Level Entry Point (depends on all above)

#### 5a. Extend `full_check_from_scratch` + CLI error output (~70 lines)
- **Path**: `flow_services_inference/src/type_service.rs` + `flow_cli/src/main.rs`
- **Why needed**: Wire the check phase into the existing pipeline and output errors to stdout
- **Dependencies**: All above tiers + `error_collator::get` (ported), `flow_common_errors::format_errors` (ported)
- **Note**: The current `full_check_from_scratch` stops after merge. Need to add: (1) init_libs call before merge, (2) check_files call after merge, (3) error collation, (4) error output via ErrorCollator.get + format_errors.
- **Complexity**: Simple — extending existing function

### Tier 6: Infrastructure That Can Be Stubbed/Simplified

These modules are referenced by the full-check path but can be simplified:

| Module | Lines | Can Stub? | Notes |
|--------|-------|-----------|-------|
| `serverEnvBuild.ml` | 90 | Yes | `make_genv` just wraps options + workers — trivial |
| `profiling_js.ml` | 1,079 | Yes | Replace with simple timing/no-op |
| `recheck_stats.ml` | ~50 | Yes | Logging only |
| `loggingUtils.ml` | 98 | Yes | Logging only |
| `flowEventLogger.ml` | large | Yes | Telemetry only |
| `monitorRPC.ml` | ~100 | Yes | Not needed for standalone check |
| `multiWorkerLwt.ml` | ~200 | Yes | Replaced by ThreadPool + map_reduce |
| `persistent_connection.ml` | ~200 | Yes | Not needed for check-only |
| `export_service.ml` | 457 | Yes | Only needed for autoimports |
| `saved_state*.ml` | various | Yes | Skip saved state for initial port |
| `obj_to_obj_hook.ml` | ~50 | Yes | Only needed for find-refs |
| `findRefs_js.ml` | ~200 | Yes | Not needed for check-only |

---

## Remaining `todo!()` and `unimplemented!()` Inventory

| Location | Context | Critical? |
|----------|---------|-----------|
| `flow_analysis/src/property_assignment.rs:23` | `eval_property_assignment()` — stubbed analysis function | No — only affects property assignment analysis |
| `flow_typing_statement/src/statement.rs:8320` | `$Flow$DebugThrow` internal test-only builtin | No — debug-only, never encountered in production code |

**Total `todo!()`**: 2
**Total `unimplemented!()`**: 0

---

## Crate Completion Summary

### Fully Ported (100%)

| Category | Crates | Total Rust Lines | Notes |
|----------|--------|-----------------|-------|
| **Typing** | 21 crates | ~163,000 | All complete, 1 debug-only todo |
| **Parser** | flow_parser | 69,504 | Complete |
| **Env Builder** | flow_env_builder, flow_env_builder_resolver | 43,049 | Complete |
| **Common** | flow_common, flow_common_errors, flow_common_ty, flow_common_utils, flow_common_leb128, flow_common_modulename, flow_common_tarjan, flow_common_cycle_hash, flow_common_xx | 24,213 | Complete |
| **Type Sig** | flow_type_sig | 34,152 | Complete (incl test lines) |
| **Analysis** | flow_analysis | 7,687 | 1 todo (property_assignment) |
| **Data structures** | flow_data_structure_wrapper, flow_aloc, flow_packed_locs, flow_lint_settings, flow_utils_* | 4,599 | Complete |
| **Parser utils** | flow_parser_utils, flow_parser_utils_output | 3,567 | Complete |
| **Imports/Exports** | flow_imports_exports | 2,048 | Complete |

### Services & Infrastructure

| Crate | Rust Lines | Completion | Key Gaps |
|-------|-----------|------------|----------|
| flow_services_inference | 3,077 | ~75% | Missing: check_files orchestration, init_from_scratch, check_files_for_init |
| flow_services_module | 1,987 | ~95% | Essentially complete |
| flow_services_coverage | 491 | 100% | Complete |
| flow_flowlib | 119 | 100% | Complete (+ generated content modules) |
| flow_heap | 1,142 | ~70% | Missing: some reader/mutator abstractions |
| flow_server_env | 719 | ~95% | error_collator fully ported |
| flow_cli | 4,093 | ~80% | Missing: check command with error output |
| flow_parsing | 1,083 | ~95% | Complete |

### Global Statistics

| Metric | Value | Change from Previous |
|--------|-------|---------------------|
| Total Rust lines | **364,736** | +2,989 |
| Total crates | **53** | +7 |
| Total `todo!()` | **2** | unchanged |
| Total `unimplemented!()` | **0** | unchanged |
| Typing subsystem | **100% ported** | unchanged |
| Parser | **100% ported** | unchanged |
| Module resolution | **~95% ported** | unchanged |
| Full-check pipeline (parse→merge) | **100% ported** | unchanged |
| Full-check pipeline (check→errors) | **~60% ported** | **up from ~0%** |

### New Crates Since Last Report

| Crate | Lines | Purpose |
|-------|-------|---------|
| flow_flowlib | 119 | Flowlib extraction (ported from flowlib.ml) |
| flow_common_cycle_hash | 361 | Cycle hashing |
| flow_common_xx | 81 | XX hashing |
| flow_typing_builtins | 198 | Builtin types |
| flow_typing_default | 84 | Default typing |
| flow_typing_exists_check | 53 | Exists checking |
| flow_typing_implicit_instantiation_check | 92 | Implicit instantiation |
| flow_typing_key | 93 | Typing keys |

---

## Recommended Porting Order

### Phase 1: Lib Loading (~170 lines of new Rust)
1. `context_heaps.ml` (38 OCaml lines) — may be trivial: master_cx is already threaded through the Rust code. At minimum, provide `add_master`/`find_master` wrappers or store on a shared state struct.
2. `init_js.ml` (156 OCaml lines) — lib file loading and initialization. The core dependency `Merge_js.merge_lib_files` is already ported. Main work: read parsed lib file ASTs from SharedMem, call `merge_lib_files`, run `scan_for_suppressions`, filter lint errors, store master context.

### Phase 2: Check Orchestration (~210 lines of new Rust)
3. `Check_files.check_files` (~120 OCaml lines from types_js.ml) — orchestrate per-file checking using already-ported `mk_check`, `mk_job`, `mk_next`. Core logic: filter dependents to check, create check jobs via map_reduce, accumulate results (errors, warnings, suppressions, coverage).
4. `check_files_for_init` (~90 OCaml lines from types_js.ml) — bridge between merge and error output. Calls `check_files`, then `update_collated_errors`. All callees are ported.

### Phase 3: Wire It Together (~70 lines of new Rust)
5. Extend `full_check_from_scratch` in `type_service.rs` — add: (a) flowlib extraction + lib parsing, (b) `init_js::init` call, (c) `check_files` call after merge, (d) `error_collator::update_collated_errors`, (e) `error_collator::get` to extract final errors.
6. Update `flow_cli/src/main.rs` `check` command — output errors in CLI/JSON format using `flow_common_errors::format_errors`.

---

## Line Count Summary

| Category | Ported (Rust lines) | Remaining (OCaml lines) | Notes |
|----------|--------------------|------------------------|-------|
| Typing core (incl statement.ml) | ~163,000 | 0 | **DONE** |
| Parser + parser utils | 73,071 | 0 | **DONE** |
| Type sig + imports/exports | 36,200 | 0 | **DONE** |
| Env builder + analysis | 50,736 | 0 | **DONE** (1 todo in property_assignment) |
| Common/utils | 28,812 | 0 | **DONE** |
| Services (parse/merge/modules) | 6,147 | 0 | **DONE** |
| Heap/coverage/cli/parsing/server_env | 7,647 | 0 | **DONE** (for current scope) |
| Error collation | 454 | 0 | **DONE** (ported since last report) |
| Flowlib extraction | 119 | 0 | **DONE** (ported since last report) |
| mk_check + job_utils | 671 | 0 | **DONE** (ported since last report) |
| **Init/lib loading** | 0 | ~170 | context_heaps + init_js |
| **Check orchestration** | 0 | ~210 | Check_files.check_files + check_files_for_init |
| **Top-level wiring** | 0 | ~70 | extend full_check + CLI |
| **Total remaining** | — | **~450** | **All orchestration/glue code** |

The critical path is: **init_js.ml (156 lines) → Check_files.check_files (~120 lines) → check_files_for_init (~90 lines) → wiring (~70 lines)**. Since the last report, ~900 lines of OCaml orchestration code have been ported (errorCollator, flowlib, mk_check, job_utils), reducing the remaining work from ~1,350 lines to ~450 lines of OCaml — entirely glue code connecting already-ported components.
