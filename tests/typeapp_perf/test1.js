/* This test ensures that the code below does not take a long time to check. If
 * this test is taking a very long time to complete, there is a bug. */

class A {}

type B<T> = A & {
  readonly a: () => B<T>;
  readonly b: () => B<T>;
  readonly c: () => B<T>;
  readonly d: () => B<T>;
  readonly e: () => B<T>;
  readonly f: () => B<T>;
  readonly g: () => B<T>;
  readonly h: () => B<T>;
  readonly i: () => B<T>;
};

declare const b: B<any>;

b as B<any>;
