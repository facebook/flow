// `declare namespace X { type T = ... }` followed by a same-name
// `interface X { ... }` (TypeScript declaration merging, common in `.d.ts`
// files like `effect/dist/dts/ManagedRuntime.d.ts`) must not crash
// `init_type_param` with a "synthetic [this] whose AssigningWrite was not
// registered" failure now that interfaces have polymorphic `this`. The crash
// triggers when the interface body has any inner type parameter (e.g. a
// function-typed property `<A>(...) => ...`), because the synthetic `this`
// tparam at the interface's name location is iterated in `mk_tparams_map`
// but the interface binding had been suppressed by the namespace's prior
// `Type{type_only_namespace=true}` registration.
declare namespace MR {
  type C<T> = T;
}

interface MR<R> { // ERROR — name-already-bound (separate pre-existing limitation; Flow's `can_coexist` rejects type-only-namespace + interface declaration merging)
  runFork: <A>(self: A) => R;
}
