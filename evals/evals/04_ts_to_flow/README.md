# Category 4 — TypeScript → Flow Conversion

Each eval hands the model a TypeScript program and asks for an idiomatic, modern
Flow port. This category is distinct from Categories 2/7, which test writing Flow
from a natural-language spec with no source: here the challenge is *converting*
existing TypeScript, including the cases where TS idioms have no 1:1 Flow
equivalent (function overloads, array covariance, generic indexed-access writes,
branded types).

## Layout

```
<name>/
├── prompt.md          # "Convert source.ts to idiomatic Flow in main.js."
├── config.json        # metadata + graders (flow_check + AST graders)
├── input/
│   ├── source.ts      # the TypeScript to convert (read by the agent; Flow
│   │                  #   ignores .ts, so it is grader-inert and never enters
│   │                  #   the gold patch — it is absent from ideal/)
│   ├── main.js        # `// @flow` + a TODO stub
│   └── .flowconfig    # React evals enable
│                      #   experimental.opaque_type_new_bound_syntax + [libs] flow-typed
└── ideal/
    └── main.js        # authored reference solution (the gold patch);
                       #   multi-file evals add more files (e.g. ids.js, money.js)
```

The gold patch is `diff(input/, ideal/)`. For multi-file evals, every file that
exists only in `ideal/` (e.g. `ids.js`) is created by the patch and by the model.

## Design notes

- **Idiomatic == modern.** Prompts say "idiomatic Flow" and never name the
  construct. React must use `component`/`hook` syntax and ref-as-prop (not
  `forwardRef`); unions are matched with `match`, not `switch`.
- **Preserve runtime behavior.** Conversions keep semantics; type-only choices
  (e.g. `ReadonlyArray` for covariance) are allowed, semantic changes are not.
- **Modern spellings are required.** `flow check` flags deprecated forms
  (`$ReadOnly`, `$ReadOnlyArray`, `$NonMaybeType`, `mixed`, `+`-variance,
  `<T: Bound>`), so a model emitting them fails. Several configs also add a
  negative AST grader forbidding `$`-prefixed utilities directly. See
  `../../FEATURE_COVERAGE.md` for the full modern-form table and per-eval
  coverage.

## Verifying

```bash
# Whole pipeline (compile + gold-patch validation, no API calls):
make validate

# A single eval against its gold patch:
python3 run_swebench.py --dry-run --flow-bin "$FLOW_BIN" --eval <name>
```

## TODO

- **Runtime (jest) grading** is not yet ported. The original Devmate suite ran
  jest bodies against the converted code; the current harness only grades types.
  A `graders/jest.sh` could execute the converted module as an additional check.
