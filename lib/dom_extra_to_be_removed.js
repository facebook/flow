/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

interface ImportMeta {
  url?: string;
}

/* Commonly available, shared between node and dom */
declare var console: {
    assert(condition: unknown, ...data: Array<any>): void,
    clear(): void,
    count(label?: string): void,
    countReset(label?: string): void,
    debug(...data: Array<any>): void,
    dir(...data: Array<any>): void,
    dirxml(...data: Array<any>): void,
    error(...data: Array<any>): void,
    _exception(...data: Array<any>): void,
    group(...data: Array<any>): void,
    groupCollapsed(...data: Array<any>): void,
    groupEnd(): void,
    info(...data: Array<any>): void,
    log(...data: Array<any>): void,
    profile(name?: string): void,
    profileEnd(name?: string): void,
    table(tabularData: { [key: string]: any, ... } | Array<{ [key: string]: any, ... }> | Array<Array<any>>): void,
    time(label?: string): void,
    timeEnd(label: string): void,
    timeStamp(label?: string): void,
    timeLog(label?: string, ...data?: Array<any>): void,
    trace(...data: Array<any>): void,
    warn(...data: Array<any>): void,
    ...
};

declare function btoa(rawString: string): string;
declare function atob(encodedString: string): string;
declare function clearInterval(intervalId: ?IntervalID): void;
declare function clearTimeout(timeoutId: ?TimeoutID): void;
declare function setTimeout<TArguments extends Array<unknown>>(
  callback: (...args: TArguments) => unknown,
  ms?: number,
  ...args: TArguments
): TimeoutID;
declare function setInterval<TArguments extends Array<unknown>>(
  callback: (...args: TArguments) => unknown,
  ms?: number,
  ...args: TArguments
): IntervalID;
declare function queueMicrotask<TArguments extends ReadonlyArray<unknown>>(
  callback: (...args: TArguments) => unknown,
): void;
interface VoidFunction {
    (): void;
}

declare opaque type TimeoutID;
declare opaque type IntervalID;
