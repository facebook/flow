class C { m = function(): void { } }
class D extends C { }

var d: interface { +m: () => void } = new D();
