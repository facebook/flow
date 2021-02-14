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

declare var o : {[string] : number};

(o : interface {[string] : number});
({x : 3} : interface {[string] : number});

declare var o2 : interface {[string] : number};
(o2["a"] : number);
(o2["b"] : boolean); // error


declare var o3 : {[string] : number};

(o3 : interface {[string] : boolean}); // error
({x : 3} : interface {[string] : boolean}); // error

(o3: interface {[number] : number}); // error
({x : 3} : interface {[number] : number}); // error

declare var o4 : {["asdf"] : number};

(o4 : interface {[string] : number}); // error

declare var o5 : {+["asdf"] : number};

(o5 : interface {+[string] : number});

declare var o6 : {[string] : number};

(o6 : interface {["asdf"] : number}); // error
