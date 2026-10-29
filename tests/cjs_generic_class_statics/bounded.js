// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

class Bounded<T extends number> {
  static id(value: T): T { return value; } // ERROR - static members cannot reference `T`
}
module.exports = Bounded;
