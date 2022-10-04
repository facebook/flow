class C {
  x: number;
  constructor () {
    this.x = 0;
  }
  foo(): number {
    return this.x;
  }
}

var c = new C();
var x: string = c.foo(); // ERROR: number =/= string

function foo(this: {y: string | number, ...}) { return this.y; }
function bar(this: {foo(): string | number, ...},) { return this.foo(); }
var o = { y: "", foo: foo, bar: bar };
var o2 = { y: 0, foo: foo, bar: bar };

o.bar();
var y: number = o2.bar();
