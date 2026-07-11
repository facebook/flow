# Relay Eval Context

Standalone Flow root with Relay type stubs for Category 3 (Real-World Project
Patterns) evals. Eval tasks test whether models can produce correct Relay + Flow
component code.

## Structure

```
relay_core/
└── context/                    # Standalone Flow root — models edit files here
    ├── .flowconfig
    ├── libdefs_xplat/          # Minimal libdefs needed by the evals
    ├── RelayHooks.js           # Declare stub for all relay hooks + PreloadedQuery type
    ├── relay-runtime/
    │   ├── package.json        # haste_commonjs:true — resolves "relay-runtime" module name
    │   └── index.js            # Declare stubs: OperationType, FragmentType, etc.
    └── *.graphql.js            # Hand-authored generated query/fragment types per eval
```

## Type design notes

### `RelayHooks.js`

`RelayHooks.js` declares `graphql` and the hooks (`usePreloadedQuery`,
`useFragment`, etc.) against `GraphQLTaggedNode`. With `relay_integration=true`
(see below), Flow resolves each `graphql` tagged template at the call site to
the matching `.graphql.js` artifact module's exports rather than to the declared
`GraphQLTaggedNode` return type.

The `useFragment` signature still infers its return type from the key's `$data`
property via indexed access (`NonNullable<TKey['$data']>`). This means:

- The `.graphql.js` fragment key type must include `+$data?: DataType`
- When calling `useFragment(fragment, key)`, Flow infers `TData` from the key

### `relay-runtime/`

Excluded from haste path scanning (`haste.paths.excludes`) so the module name
`relay-runtime` resolves through the package.json's `haste_commonjs` mechanism.

### relay_integration

`relay_integration=true` is enabled, to match www. With it on, a `graphql`
tagged template literal resolves to the corresponding `.graphql.js` module's
exports — so the model does not need to import query/fragment types from the
generated files explicitly.

For that resolved module to satisfy the hooks' `GraphQLTaggedNode` parameter,
each artifact stub exports a runtime `kind` sentinel
(`export const kind: 'Request' = 'Request'` for queries,
`export const kind: 'Fragment' = 'Fragment'` for fragments), and
`GraphQLTaggedNode` in `relay-runtime/` is defined as `{+kind: string, ...}`
(read-only, matching the `const` export).

## Adding a new eval prompt

1. Author `.graphql.js` type files in `context/` for the new scenario
2. Write the prompt in `candidate_prompts/NN_name.md`
3. Optionally write ideal component files in `context/` and verify with `flow full-check`
4. Remove ideal files before committing (or move to a separate `ideals/` dir)

## Verifying context is clean (0 errors without model files)

```bash
cd context
flow full-check --show-all-errors
```
