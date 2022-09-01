function f(this: number) {
  (this: number); // OK
}

f.a = 1;
f.b = () => 1;

(f.a: number); // OK
(f.b(): number); // OK

(f.a: empty); // ERROR
(f.b(): empty); // ERROR

g.v = true;

function g() {}

(g.v: boolean); // OK

f.c = ""; // OK
(f.c: string); // OK
f.c = 1; // ERROR

g.w = 1; // OK
(g.w: number); // OK

declare var b: boolean;
if (b) {
  f.xxx = 1; // ERROR
}

f.d = (): string => f.c; // OK

export default function n() {}
export function m() {}

n.displayName = 'a';
m.a = 1;
(n.displayName: string);
(m.a: number);
