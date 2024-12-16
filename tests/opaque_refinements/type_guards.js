// @flow

type A<T> = { +$$type: 1, value: T } | { +$$type: 2, value: T };
type B<T> = A<T>;
declare opaque type O<T>;
type C<T> = B<T> | O<T>;

declare function isB<T>(compute: C<T>): compute is B<T>;

function test1(value: C<mixed>): void {
  if (isB(value)) {
    const _1 = value as B<mixed>;
    const _2: B<mixed> = value;
  }
}

function test2() {
    type StringC = C<string>;
    type StringB = B<string>;
    function b(value: StringC) {
        if (isB(value)) {
            const _1 = value as StringB;
            const _2: StringB = value;
        }
    }
}
