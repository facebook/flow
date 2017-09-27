/**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare module flux {
  declare class Dispatcher<P> {
    register(callback: (payload: P) => void): string;
    unregister(id: string): void;
    waitFor(ids: Array<string>): void;
    dispatch(payload: P): void;
    isDispatching(): boolean;
  }
}
