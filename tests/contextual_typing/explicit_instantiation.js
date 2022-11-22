// @flow

import * as React from "React";

declare function f<T>(T => void): void;
declare class A<T> {
  constructor(T => void): void;
}

f<string>((v) => { (v: string) }); // ok
new A<string>((v) => { (v: string) }); // ok
new A<any>(v => {}); // ok
