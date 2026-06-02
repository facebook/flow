/* This test ensures that the code below does not take a long time to check. If
 * this test is taking a very long time to complete, there is a bug. */

class A {}

type B<T> = A & {
  readonly a: (x: B<T>) => void;
  readonly b: (x: B<T>) => void;
  readonly c: (x: B<T>) => void;
  readonly d: (x: B<T>) => void;
  readonly e: (x: B<T>) => void;
  readonly f: (x: B<T>) => void;
  readonly g: (x: B<T>) => void;
  readonly h: (x: B<T>) => void;
  readonly i: (x: B<T>) => void;
};

declare const b: B<any>;

b as B<any>;
