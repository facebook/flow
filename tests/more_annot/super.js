class C { m = function(): void { } }
class D extends C { }

var d: interface { readonly m: () => void } = new D();
