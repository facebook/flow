const f = () => {
  (this: number); // ERROR
};
f.a = 1;

(f.a: number); // OK
(f.a: empty); // ERROR

const g = () => {};
g.v = true;

(g.v: boolean); // OK

f.c = ""; // OK
(f.c: string); // OK

g.w = 1; // OK
(g.w: number); // OK
