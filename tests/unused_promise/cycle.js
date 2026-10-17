declare class A extends B {}
declare class B extends A {}
declare function a(): A;

interface I<T> extends I<T> {}
declare function i(): I<number>;

async function cycle() {
  a(); // ok, terminates on recursive extends
  i(); // ok, terminates on recursive generic extends
}
