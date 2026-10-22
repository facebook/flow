/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare const implementation: (s: string) => number;
declare function API(s: string): number;
declare namespace API {
  export { implementation as call };
}
export = API;
