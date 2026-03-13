type T = {[string]?: number};
declare const t: T;
t["foo"] as number; // ERROR - undefined is not number

type S = {readonly [string]?: number};
declare const s: S;
s["foo"] as number; // ERROR - undefined is not number

declare class C {
  [string]?: number;
}
declare const c: C;
c["foo"] as number; // ERROR - undefined is not number

interface I {
  [string]?: number;
}
declare const i: I;
i["foo"] as number; // ERROR - undefined is not number
