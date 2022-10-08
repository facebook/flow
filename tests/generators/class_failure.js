// generalization of failure in class.js

class GeneratorExamples<X> {
  *infer_stmt(): Generator<number, string, void> {
    var x: boolean = yield 0; // error: void ~> boolean
    return "";
  }
}

var examples = new GeneratorExamples<mixed>();

for (var x of examples.infer_stmt()) { (x : string) } // error: number ~> string

var infer_stmt_next = examples.infer_stmt().next(0).value; // error: number ~> void

if (typeof infer_stmt_next === "undefined") {
} else if (typeof infer_stmt_next === "number") {
} else {
  (infer_stmt_next : boolean) // error: void ~> boolean
}
