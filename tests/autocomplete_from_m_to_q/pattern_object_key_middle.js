// @flow

type T = { foobar: string };

function f(x: T) {
  const { foo } = x;
//         ^
}
