/* @flow */

function test1(): string {
  return bar();

  function bar() {
    return 0;
  }
}
