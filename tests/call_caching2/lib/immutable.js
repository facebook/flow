// Copyright (c) Meta Platforms, Inc. and affiliates.

declare class Array<T> { }

declare class Iterable<S> {
  static <V,Iter:Iterable<V>>(iter: Iter): Iter;
  static <T>(iter: Array<T>): Iterable<T>;
  size: number;
}
