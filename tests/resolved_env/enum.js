// @flow
enum W {
  A, B
}

var x: W = W.A;
var y: W = 10; // err
var z: W.A; // err
