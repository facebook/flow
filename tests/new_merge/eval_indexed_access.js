// @flow

type Foo1 = {foo: {bar: {baz: number}}};
declare const bar1: Foo1['foo']['bar'];
export const baz1 = bar1.baz;

type Foo2 = {[string]: {[string]: {[string]: number}}};
declare const bar2: Foo2[string][string];
declare const key: string;
export const baz2 = bar2[key];
