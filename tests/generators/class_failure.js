// generalization of failure in class.js

class GeneratorExamples<X> {
  *infer_stmt() {
    var x: ?boolean = yield 0;
    return "";
  }
}

var examples = new GeneratorExamples();

for (var x of examples.infer_stmt()) { (x : string) } // error: number ~> string

var infer_stmt_next = examples.infer_stmt().next(0).value; // this should be an
                                                           // error, but we miss
                                                           // it: see class.js
                                                           // for details

if (typeof infer_stmt_next === "undefined") {
} else if (typeof infer_stmt_next === "number") {
} else {
  (infer_stmt_next : boolean) // error: string ~> boolean
}
