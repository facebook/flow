// @flow

import type { PromiseLike as RemotePromiseLike } from './export';

// Same definition as in export.js
export interface PromiseLike<T> {
  then<TResult1 = T, TResult2 = empty>(
    onfulfilled?: (value: T) => TResult1 | PromiseLike<TResult1>
  ): PromiseLike<TResult1 | TResult2>;
}

declare var p: PromiseLike<mixed>;
// This check exercises Flow's termination strategies
(p: RemotePromiseLike<mixed>);
