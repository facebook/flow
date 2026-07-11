/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

type ElementType<T> = T extends Array<infer U> ? U : T;

type Resolved<T> = T extends Promise<infer U> ? U : T;

type Tagged<T> = T extends string
  ? { kind: "text"; value: T }
  : { kind: "num"; value: T };

type Token = Tagged<string | number>;

declare function fetchScores(): Promise<number[]>;

type Score = ElementType<Resolved<ReturnType<typeof fetchScores>>>;

function add(a: Score, b: Score): Score {
  return a + b;
}

function describe(token: Token): string {
  switch (token.kind) {
    case "text":
      return `text(${token.value.toUpperCase()})`;
    case "num":
      return `num(${token.value.toFixed(2)})`;
  }
}

const tokens: Token[] = [
  { kind: "text", value: "hi" },
  { kind: "num", value: 3.14159 },
];

console.log(tokens.map(describe).join(", "), add(1, 2));
