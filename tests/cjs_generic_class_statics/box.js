// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

class Box<T> {
  static id(value: T): T { return value; } // ERROR - static members cannot reference `T`
  static count: number = 1;
  static echo<U>(value: U): U { return value; }
}
module.exports = Box;
