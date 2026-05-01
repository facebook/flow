function f(this: number) {
  this as number; // OK
}

f.a = 1;
f.b = () => 1;

f.a as number; // OK
f.b() as number; // OK

f.a as empty; // ERROR
f.b() as empty; // ERROR

g.v = true;

function g() {}

g.v as boolean; // OK

f.c = ""; // OK
f.c as string; // OK
f.c = 1; // ERROR

g.w = 1; // OK
g.w as number; // OK

declare var b: boolean;
if (b) {
  f.xxx = 1; // ERROR
}

f.d = (): string => f.c; // OK

export default function n() {}
export function m() {}

n.displayName = 'a';
m.a = 1;
n.displayName as string;
m.a as number;
