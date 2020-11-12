//@flow

let a: number => number = (x) => 3; // annot available

const b: number => void = (x) => { // This x has an annot available
  const b = ((x) => 3)(3); // This x does not
  const c = (x) => 3; // Neither does this one
};

var c: number => number = function(x) { return 3 }; // annot available
