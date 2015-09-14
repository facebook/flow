function *infer_return() {}
var infer_return_value = infer_return().return("").value;
if (typeof infer_return_value === "undefined") {
} else {
  (infer_return_value : number); // error: string ~> number
}

function *explicit_return() {
  return 0;
}
explicit_return().return(""); // error: string ~> number

function *annot_return(): Generator<void, string, void> {
  // TODO: No explicit return currently allowed. Should it be?
}
annot_return().return(0); // error: number ~> string

declare var declared_return: Generator<void, string, void>;
declared_return.return(0); // error: number ~> string
