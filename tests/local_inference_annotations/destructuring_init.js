//@flow

let [a]: number => number = (x) => {
  const d = (x) => 3;
  return 3; // annot available
};

// Errors: x and y are not on the statics of the function.
const {x: b, y: c}: number => void = (x) => { // This x has an annot available
  const b = ((x) => 3)(3); // This x does not
  const c = (x) => 3; // Neither does this one
};
