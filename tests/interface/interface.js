declare class C { x: number; }

var x: string = new C().x;

interface I { x: number; }

var i = new I(); // error

function testInterfaceName(o: I) {
  (o.name: string); // error, name is static
  (o.constructor.name: string); // ok
}


declare var y : { x? : string};

(y : interface { x? : string });

({ x : 3 } : interface { x : string | number });

interface J {
  x?: number;
}

class D {
  x: number;
}

(new D : J); // error, x is invariant
