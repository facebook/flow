function f1(x: string): string extends infer V ? V : empty %checks { return x; } // okay
function f2(x: string): (string extends infer V ? V : empty) extends infer V ? V : empty %checks { return x; } // okay
function f3<V>(x: V): V extends infer V ? V : empty %checks { return x; } // error
