// A union member with a property whose type is disjoint from the guard's is dropped.

declare interface A { type: 'A'; a: number }
declare interface B { type: 'B'; b: string }

function interfaces(x: A | B) {
  declare function isA(x: unknown): x is A;
  if (isA(x)) {
    x as A; // okay
    x as B; // error A ~> B
  }
}

function non_literal_props(x: {val: number} | {val: string}) {
  declare function isNum(x: unknown): x is {val: number, ...};
  if (isNum(x)) {
    x as {val: number}; // okay
    x as {val: string}; // error number ~> string
  }
}

function class_and_interface() {
  class C { kind: 'c' = 'c'; }
  declare interface HasKindD { kind: 'd' }
  declare const x: C | HasKindD;
  declare function isD(x: unknown): x is HasKindD;
  if (isD(x)) {
    x as HasKindD; // okay
    x as C; // error HasKindD ~> C
  }
}

function optional_props(x: {val?: number} | {val: string}) {
  declare function isNum(x: unknown): x is {val?: number, ...};
  if (isNum(x)) {
    x as {val?: number}; // error {val?: number, ...} & {val: string} ~> {val?: number}
  }
}

// A write-only property says nothing about the value read.
function write_only_props(x: {writeonly val: string, ...} | {val: number}) {
  declare function isNum(x: unknown): x is {val: number, ...};
  if (isNum(x)) {
    x as {val: number}; // error {writeonly val: string, ...} & {val: number, ...} ~> {val: number}
  }
}

function getters() {
  declare interface GetA { get kind(): 'a' }
  declare const x: GetA | {kind: 'b'};
  declare function isB(x: unknown): x is {kind: 'b', ...};
  if (isB(x)) {
    x as {kind: 'b'}; // okay
  }
}

// Members of a union guard that are disjoint from the refined type are dropped.
function union_guard(x: {val: string}) {
  declare function isG(x: unknown): x is {val: string, extra: number, ...} | {val: number, ...};
  if (isG(x)) {
    x.extra as number; // okay
  }
}

import type {FragmentType} from './empty_opaque_exports';

// A property of a type without values does not make objects disjoint.
function uninhabited_props() {
  declare opaque type X$fragmentType: FragmentType;
  type X$data = {readonly id: string, readonly $fragmentType: X$fragmentType};
  type A = {type: 'a', data: X$data};
  type B = {type: 'b', data: X$data};
  declare const x: A | B;
  declare function isA(x: unknown): x is {type: 'a', data: X$data, ...};
  if (isA(x)) {
    x as A; // okay
    x.type as 'a'; // okay
  }
}
