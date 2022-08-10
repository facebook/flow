// @flow

let x = null;
function foo() {
  x = 0;
}
function bar() { }
x = 1;
bar();
(x: number); // error in constrain_writes mode
