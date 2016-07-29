// @flow

declare class C {
  bar(n1: number, n2: number): number;
  bar(s1: string, s2: string): string;
}

function foo(c: C, x: any): string {
  let y = x.y;
  return c.bar(0, y); // should be able to select first case and error
}
