// Copyright (c) Meta Platforms, Inc. and affiliates.

// These annotations are copy/pasted from the built-in Flow definitions for
// Native Promises (https://phabricator.fb.com/P19792689),
// with www-specific additions added in.

// Any definitions here will override similarly-named ones in
// library files declared earlier, including default flow libs.

declare class Promise<+R> { // intentional-libdef-override
  constructor(callback: (
    resolve: (result?: Promise<R> | R) => void,
    reject: (error?: any) => void
  ) => void): void;

  then<U>(
    onFulfill?: ?(value: R) => Promise<U> | ?U,
    onReject?: ?(error: any) => Promise<U> | ?U
  ): Promise<U>;

  done<U>(
    onFulfill?: ?(value: R) => void,
    onReject?: ?(error: any) => void
  ): void;

  catch<U>(
    onReject?: (error: any) => ?Promise<U> | U
  ): Promise<U>;

  static resolve<T>(object?: Promise<T> | T): Promise<T>;
  static reject<T>(error?: any): Promise<T>;

  // Non-standard APIs
  finally<U>(
    onSettled?: ?(value: any) => Promise<U> | U
  ): Promise<U>;

  static cast<T>(object?: T): Promise<T>;
  static all<T>(
    promises: Array<?Promise<T> | T>,
  ): Promise<Array<T>>;
  static race<T>(promises: Array<Promise<T>>): Promise<T>;

  static allObject<T: Object>(
    promisesByKey: T
  ): Promise<{[key: $Keys<T>]: any}>;
}
