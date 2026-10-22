// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

class Bounded<T extends number> {
  static id(value: T): T { return value; }
}
module.exports = Bounded;
