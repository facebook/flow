/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare class API { field: number; }
declare namespace API {
  export { API as Member };
  export const value: number;
}
export = API;
