const f = function (this: number) {
  (this: number); // OK
};
f.a = 1;

(f.a: number); // OK
(f.a: empty); // ERROR

const g = function m() {};
g.v = true;

(g.v: boolean); // OK

f.c = ""; // OK
(f.c: string); // OK

g.w = 1; // OK
(g.w: number); // OK
