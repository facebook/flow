//@flow

var x = 42;
x = "true";

var y = 42;
if (x) {
  y = "hello world";
}

(42: string); // should still have some errors!
