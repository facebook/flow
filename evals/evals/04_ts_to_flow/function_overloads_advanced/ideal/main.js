/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Box<T> = {
  value: T,
  get(): T,
};

function box<T>(value: T): Box<T> {
  return {
    value,
    get() {
      return value;
    },
  };
}

type Theme = {
  color: string,
  spacing: number,
  rounded: boolean,
};

function getToken<K extends keyof Theme>(theme: Theme, key: K): Theme[K] {
  return theme[key];
}

type Identifiable = {
  id: number,
  ...
};

function indexById<T extends Identifiable>(items: ReadonlyArray<T>): Map<number, T> {
  const map = new Map<number, T>();
  for (const item of items) {
    map.set(item.id, item);
  }
  return map;
}

const theme: Theme = {color: 'blue', spacing: 8, rounded: true};

const stringBox = box('hello');
const numberBox = box(42);
const color: string = getToken(theme, 'color');
const spacing: number = getToken(theme, 'spacing');

type User = {
  id: number,
  name: string,
};
const users: Array<User> = [
  {id: 1, name: 'Alice'},
  {id: 2, name: 'Bob'},
];
const byId = indexById(users);

console.log(stringBox.get(), numberBox.get(), color, spacing, byId.get(1)?.name);
