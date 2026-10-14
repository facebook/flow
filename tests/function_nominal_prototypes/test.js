const fn = (x: number): number => x;
fn as Function;
fn as Object;

function identity<T>(value: T): T {
  return value;
}
identity as Function;
identity as Object;

class C {}
C as Function;
C as Object;
new C() as Object;

class D {}
fn as D; // ERROR
C as D; // ERROR
new C() as Function; // ERROR
({}) as Function; // ERROR
42 as Function; // ERROR

const bound: Function = fn.bind(null);
const object: Object = fn;

declare const bytes: ArrayBuffer;
WebAssembly.instantiate(bytes, {env: {fn, identity}});

window.setTimeout(fn, 0, 1);
window.setInterval(fn, 0, 1);
