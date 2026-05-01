declare class C { x: number; }

var x: string = new C().x;

interface I { x: number; }

var i = new I(); // error

function testInterfaceName(o: I) {
  o.name as string; // error, name is static
  o.constructor.name as string; // ok
}


declare var y : { x? : string};

y as interface { x? : string };

({ x : 3 } as interface { x : string | number });

interface J {
  x?: number;
}

class D {
  x: number;
}

(new D) as J; // error, x is invariant

declare var o : {[string] : number};

o as interface {[string] : number};
({x : 3} as interface {[string] : number});

declare var o2 : interface {[string] : number};
o2["a"] as number;
o2["b"] as boolean; // error


declare var o3 : {[string] : number};

o3 as interface {[string] : boolean}; // error
({x : 3} as interface {[string] : boolean}); // error

o3 as interface {[number] : number}; // error
({x : 3} as interface {[number] : number}); // error

declare var o4 : {["asdf"] : number};

o4 as interface {[string] : number}; // error

declare var o5 : {+["asdf"] : number};

o5 as interface {+[string] : number}; // OK

declare var o6 : {[string] : number};

o6 as interface {["asdf"] : number}; // error
