/**
 * @flow
 */

class ClassFoo1 {
  returnsANumber(): number { return 42; }
}

export default ClassFoo1;
export function givesAFoo1(): ClassFoo1 { return new ClassFoo1(); };
