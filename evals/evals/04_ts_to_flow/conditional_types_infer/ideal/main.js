/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type ElementType<T> = T extends Array<infer U> ? U : T;

type Resolved<T> = T extends Promise<infer U> ? U : T;

type Tagged<T> = T extends string
  ? {kind: 'text', value: T}
  : {kind: 'num', value: T};

type Token = Tagged<string | number>;

declare function fetchScores(): Promise<Array<number>>;

type Score = ElementType<Resolved<ReturnType<typeof fetchScores>>>;

function add(a: Score, b: Score): Score {
  return a + b;
}

function describe(token: Token): string {
  return match (token) {
    {kind: 'text', const value} => `text(${value.toUpperCase()})`,
    {kind: 'num', const value} => `num(${value.toFixed(2)})`,
  };
}

const tokens: Array<Token> = [
  {kind: 'text', value: 'hi'},
  {kind: 'num', value: 3.14159},
];

console.log(tokens.map(describe).join(', '), add(1, 2));
