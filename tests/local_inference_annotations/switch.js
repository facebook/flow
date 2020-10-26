//@flow

// If the entire expression has an annot available then we should be able to push
// the annotation of the return type down to the return expression, even
// through switch statements 
let a = ((x => {
  switch ((x) => true){ // Guards are not covered by the annot
    case true:
      return y => 3;
    case false: {
      const z = (x) => 3; // Non-return statements are not covered by the annot
      return y => 3;
    }
    default:
      return y => 3 // default cases are covered, too!
  };
}): number => number => number);
