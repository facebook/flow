/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// `declare namespace X { type T = ... }` followed by a same-name
// `interface X { ... }` (TypeScript declaration merging, common in `.d.ts`
// files like `effect/dist/dts/ManagedRuntime.d.ts`) must not crash
// `init_type_param` with a "synthetic [this] whose AssigningWrite was not
// registered" failure. The inner type parameter in the interface body
// exercises the crashing path. Flow does not yet support this declaration
// merge and reports `libdef-override`.
declare namespace MR { // ERROR — libdef-override
  type C<T> = T;
}

interface MR<R> {
  runFork: <A>(self: A) => R;
}
