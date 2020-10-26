//@flow


function f(x: number => number, y: any) {}

f((x) => 3, (x: number) => 3);

f((x) => {
  const a = (x) => 3; // Required annot
  return 3;
  // Error, function is not iterable. But no annotation required!
}, ...((x) => 3));

function g(x: number) {}
g(3); // Ok
