//@flow

// If the entire expression has an annot available then we should be able to push
// the annotation of the return type down to the return expression, even
// through for loops 
let a = ((x => {
  for ((x) => true; (x) => true; (x) => true) { // init, test, and update are all not covered by the annot 
    const x = (y) => 3; // non-return, not covered!
    return y => 3; // Covered by the annot
  }
  return (y) => 3;
}): number => number => number);

let b = ((x => {
  // Blocks are not necessary
  for (;;)
  return (y) => 3;

  return (y) => 3;
}): number => number => number);
