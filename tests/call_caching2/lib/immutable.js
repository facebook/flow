// Copyright (c) Meta Platforms, Inc. and affiliates.

declare class MyIterable<S> {
  static <V,Iter:MyIterable<V>>(iter: Iter): Iter;
  static <T>(iter: Array<T>): MyIterable<T>;
  size: number;
}
