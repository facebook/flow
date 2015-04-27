/**
 * @flow
 */

class ClassFoo2 {
  returnsANumber(): number { return 42; }
}

export {ClassFoo2};
export function givesAFoo2(): ClassFoo2 { return new ClassFoo2(); };
