declare class C { x: number; }

var x: string = new C().x;

interface I { x: number; }

var i = new I(); // error
