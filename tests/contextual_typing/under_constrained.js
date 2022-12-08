// @flow

function test1() {
  declare var x: (mixed) => void;
  x(y => {}); // error missing annot
}

function test2() {
  declare var x: (?mixed) => void;
  x(y => {}); // error missing annot
}

function test3() {
  declare var x: (any) => void;
  x(y => {}); // okay due to any propagation
}
