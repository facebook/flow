/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

export type MyType = number;
export interface MyInterface { x: string }
declare namespace MyNamespace {
  declare const x: number;
}
export type { MyNamespace };

type LocalType = string;
export { LocalType };

interface LocalInterface { y: number }
export { LocalInterface };

export const myValue: number = 42;

export class MyClass {
  x: number;
  constructor() { this.x = 1; }
}

export enum MyEnum {
  A = 'a',
  B = 'b',
}
