class C { m() { } }
class D extends C { }

var d: interface { +m: () => void } = new D();
