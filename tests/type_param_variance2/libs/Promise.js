// Copyright 2004-present Facebook. All Rights Reserved.

// These annotations are copy/pasted from the built-in Flow definitions for
// Native Promises (https://phabricator.fb.com/P19792689),
// with www-specific additions added in.
declare module "Promise" {
  // Use the name "PromisePolyfill" so that we don't get confusing error
  // messages about "Using Promise instead of Promise".
  declare class PromisePolyfill<R> {
    constructor(callback: (
      resolve: (result?: PromisePolyfill<R> | R) => void,
      reject: (error?: any) => void
    ) => void): void;

    then<U>(
      onFulfill?: ?(value: R) => PromisePolyfill<U> | ?U,
      onReject?: ?(error: any) => PromisePolyfill<U> | ?U
    ): PromisePolyfill<U>;

    done<U>(
      onFulfill?: ?(value: R) => void,
      onReject?: ?(error: any) => void
    ): void;

    catch<U>(
      onReject?: (error: any) => ?PromisePolyfill<U> | U
    ): PromisePolyfill<U>;

    static resolve<T>(object?: PromisePolyfill<T> | T): PromisePolyfill<T>;
    static reject<T>(error?: any): PromisePolyfill<T>;

    // Non-standard APIs
    finally<U>(
      onSettled?: ?(value: any) => PromisePolyfill<U> | U
    ): PromisePolyfill<U>;

    static cast<T>(object?: T): PromisePolyfill<T>;
    static all<T>(
      promises: Array<?PromisePolyfill<T> | T>,
    ): PromisePolyfill<Array<T>>;
    static race<T>(promises: Array<PromisePolyfill<T>>): PromisePolyfill<T>;

    static allObject<T: Object>(
      promisesByKey: T
    ): PromisePolyfill<{[key: $Enum<T>]: any}>;
  }

  // Don't "declare class exports" directly, otherwise in error messages our
  // show up as "exports" instead of "Promise" or "PromisePolyfill".
  declare var exports: typeof PromisePolyfill;
}
