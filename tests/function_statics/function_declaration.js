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

g.w = 1; // OK
(g.w: number); // OK
