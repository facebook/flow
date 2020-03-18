const f = <T>(x: T) => (y: T) => y;
const g = <T>(x: T): (T => T) => (y: T) => y;

(f(0)(1): number); // errors: number ~> T (empty), T (mixed) ~> number
(g(0)(1): number); // ok
