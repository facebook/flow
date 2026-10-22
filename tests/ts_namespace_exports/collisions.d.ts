/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

type Sig = (s: string) => number;
// A same-named value member prevents the namespace from merging with a later function.
declare namespace sameName {
  export const sameName: Sig;
  export { sameName as default };
}
declare function sameName(s: string): number; // ERROR: name already bound
export = sameName;
