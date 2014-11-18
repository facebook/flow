/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
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
