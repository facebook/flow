# Flow AI Evals

A suite of LLM coding evaluations for [Flow](https://flow.org/), a typed
dialect of JavaScript.

Each eval gives a model a small, realistic coding task that exercises a
Flow-specific type-system feature (refinements, exact objects, variance,
pattern matching, enums, component syntax, TypeScript-to-Flow conversion,
`.flowconfig`/libdef tooling, and more). The model edits files in a working
directory; the result is then graded automatically by type-checking it with
Flow and inspecting the produced AST.

The format is [SWE-bench](https://www.swebench.com/)-style: eval directories are
compiled into a JSONL of instances, each instance is run (edits applied by the
model, or the reference "gold" patch applied in dry-run mode), and a generated
TAP grading script decides pass/fail.

## How it works

Each eval lives in `evals/<category>/<name>/`:

- `prompt.md` — the task description shown to the model. It describes *what* the
  code should do, never *how* to express it in Flow.
- `config.json` — metadata (name, category, tags, difficulty) and any
  eval-specific graders.
- `input/` — the starting files (usually a `main.js` with `// TODO: Implement`,
  or code to refactor, plus a `.flowconfig`).
- `ideal/` — a sparse overlay containing only the files that differ from
  `input/`: the reference solution used as the gold patch in dry-run mode.

`compile_swebench.py` diffs `input/` against `ideal/` to produce each instance's
gold patch and generates its grading script. `run_swebench.py` sets up a temp
working directory, invokes the model (or applies the gold patch), and runs the
graders, reporting per-eval pass/fail.

## Categories

| Directory | Focus |
| --- | --- |
| `evals/01_error_fixing` | Fix code that Flow rejects |
| `evals/02_unique_features` | Flow-specific features (match, enums, variance, components, …) |
| `evals/03_project_patterns` | Multi-file / project-shaped tasks |
| `evals/04_ts_to_flow` | Convert TypeScript to idiomatic Flow |
| `evals/05_code_generation` | Write new typed code from a spec |
| `evals/06_config_tooling` | `.flowconfig`, libdefs, suppressions, lint config |

## Requirements

- **Python 3.9+** — standard library only, no third-party packages
- **Node.js and npm** — used to install the Flow binary; you do **not**
  need to build Flow from source
- **A POSIX shell environment** — the graders are `bash` scripts that use
  common Unix tools (`jq`, `grep`, `cmp`, `patch`)
- *(optional)* the **`claude` CLI** and an Anthropic API key — only needed to run
  evals against a model; dry-run/validation (`make validate`) needs neither

## Setup

```sh
npm install
```

This installs the [`flow-bin`](https://www.npmjs.com/package/flow-bin) package,
which provides a prebuilt `flow` binary at `node_modules/.bin/flow`. That is the
default binary the harness uses — no Flow build step is required.

The `make` targets run `npm install` automatically the first time if
`node_modules/` is missing, so you can also just run `make validate` directly.
(Passing your own `FLOW_BIN`, below, skips the install.)

### Using a locally built Flow binary

When developing Flow itself, point the harness at your own binary with
`FLOW_BIN` (Makefile) or `--flow-bin` (scripts):

```sh
make run FLOW_BIN=/path/to/flow
python3 run_swebench.py --flow-bin /path/to/flow --dry-run
```

## Running

```sh
make validate          # compile + apply gold patches + grade (no API calls)
make list              # list all eval names
make run ARGS="--model claude-sonnet-5"   # run all evals against Claude
make clean             # remove build/
```

`make validate` (alias for `make dry-run`) is the fastest way to confirm every
eval is well-formed: it applies each reference solution and checks that all
graders pass, without calling any model.

Filters can be passed through `ARGS` (repeatable) to run a subset:

```sh
make dry-run ARGS="--category 04_ts_to_flow"
make dry-run ARGS="--tag match"
make dry-run ARGS="--eval variance_advanced"   # substring or glob
make run     ARGS="--model claude-sonnet-5 --limit 20"
```

`make run` runs 4 evals in parallel; pass `-j N` through `ARGS` to change the
parallelism. `run_swebench.py` itself defaults to sequential (`-j 1`).

Results are written to `build/swebench/results.json`. When running against a
model, each eval's raw trajectory (`stream-json`) is saved in its run directory;
the path is printed at the end of the run.

## Grading

Every eval is graded by a set of shell graders in `graders/`, composed into a
single TAP script per eval. The common ones:

- **`flow_check`** — the solution must type-check with zero Flow errors.
- **`no_extra_flow_errors`** — penalizes trajectories that repeatedly hit Flow
  errors instead of reasoning the types through.
- **`no_tsc`** — these are Flow tasks; invoking `tsc` fails the eval.
- **`file_modified`** — the target file must actually change.
- **AST graders** (`ast_query`, `contains_ast_node_type`, `no_any`,
  `no_commonjs`) — assert (or forbid) specific AST shapes via `flow ast` + `jq`,
  so an eval that tests a feature confirms the feature was actually used.
- **`file_contains` / `no_flowfixme`** — text-level checks for pragmas,
  suppressions, and forbidden escape hatches.

Baseline graders are applied per category automatically (see
`compile_swebench.py`); each eval only lists the extra graders it needs.

## Adding an eval

1. Create `evals/<category>/<name>/` with `prompt.md`, `config.json`, `input/`,
   and `ideal/`.
2. Follow the design principles:
   - The prompt describes behavior, not the Flow syntax under test.
   - Branches should do real work (computation, calls), not literal lookups.
   - The reference solution must meaningfully use the feature being tested.
   - Each eval should test a distinct concept — check for overlap first.
   - Graders should reject wrong solutions without over-fitting to one valid
     approach.
   - Prefer realistic scenarios over textbook examples.
3. Run `make validate ARGS="--eval <name>"` to confirm it compiles and the gold
   patch passes every grader.

## License

Flow AI Evals is MIT-licensed. See the
[LICENSE](../LICENSE) file in the repository root.
