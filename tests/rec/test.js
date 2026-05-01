class P<X> { x: X; } // this is like Promise

type Pstar<X> = X | Pstar<P<X>>; // this is like Promise*

var p: P<number> = new P;
p.x as string; // error

var pstar: Pstar<number> = 0; // OK
pstar as number; // error, but limit potentially unbounded number of errors!
                 // e.g., P<number> ~/~ number, P<P<number>> ~/~ number, ...

pstar = p; // OK
pstar.x as string; // error

pstar = new P as P<P<number>>; // OK
pstar.x as string; // error
