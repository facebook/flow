// @flow

function foo(x : $Tainted<string>, o : Object) {
  // Error
  o.f(x);
}
function foo1(x : $Tainted<string>, o : {f : (y : $Tainted<string>) => void}) {
  o.f(x);
}
function foo2(o1 : Object, o2 : {t : $Tainted<string>}) {
  o1.f(o2.t);
}
function foo3<T>(x : $Tainted<T>, o : {f : (y : $Tainted<T>) => void}) {
  o.f(x);
}
