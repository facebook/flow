/* @flow */


import {suite, test} from '../../../tsrc/test/Tester';

export default suite(({addCode}) => [
  /* Up to v0.28.0, we had a bug where we missed a type param subst. It was
   * hard to hit, though, so we didn't notice for ages */
  test('Oh boy, this was hard to repro', [
    addCode(`
      class Foo<T> {}

      class Bar<T, F: Foo<T>> {}

      function foo<T, F: Foo<T>>(bar: Bar<T, F>) {
        baz(bar);
      }

      function baz(b: Bar) {}
  `).noNewErrors(),
  ]),
]);
