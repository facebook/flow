/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

interface Serializable {
  serialize(): string;
}

class JsonValue implements Serializable {
  data: unknown;
  constructor(data: unknown) {
    this.data = data;
  }
  serialize(): string {
    return JSON.stringify(this.data);
  }
}

function serializeAll<T>(items: Array<T>): Array<string> {
  return items.map(item => item.serialize());
}

function identity<T>(value: T): T {
  const result: T = value;
  return result;
}

function transform<T>(input: T, fn: (x: T) => T): T {
  return fn(input);
}

function main(): void {
  const values = [new JsonValue(1), new JsonValue('hello')];
  const serialized = serializeAll(values);

  const num = identity(42);
  const str = identity('hello');

  const doubled = transform(5, (x) => x * 2);
}
