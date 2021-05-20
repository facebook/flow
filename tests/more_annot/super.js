class C { m = function() { } }
class D extends C { }

var d: interface { +m: () => void } = new D();
