/* @flow */

function highlander(howMany: 1): string {
  return "there can be only one!";
}

highlander(1);
highlander(2); // error


type Foo = 1 | 2

function bar(num: Foo): number {
  return num + 1;
}

bar(1);
bar(2);
bar(3); // error
