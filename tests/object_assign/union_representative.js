enum En {A, B}
declare var tgt: {a: En};
declare var src: {a: 'A' | 'B'};
Object.assign(tgt, src); // error: "A" ~> En, in at least one member of `"A" | "B"`
