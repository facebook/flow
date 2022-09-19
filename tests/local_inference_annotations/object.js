//@flow

const annotated: any = {
  foo(x) {
    const a = (x) => 3; // Missing annot
    return (x) => 3;
  },
  bar: (x) => {
    const b = (x) => 3; // Missing annot
    return (x) => 3;
  },
  deep: {
    inner: (x) => 3,
  },
  "literal": (x) => 3,
  __proto__: (x) => {
    const c = (x) => 3; // Missing annot
    return (x) => 3;
  },
  "literal function"(x) {
    const d = (x) => 3; // Missing annot
    return (x) => 3;
  },
  ["computed key"]: (x) => 3,
  ["computed key function"](x) {
    const e = (x) => 3; // Missing annot
    return (x) => 3;
  },
  ...{spreadsWorkToo(x) {
    const f = (x) => 3; // Missing annot
    return (x) => 3;
  }},
  get gettersAreSupported() {
    const g = (x) => 3; // Missing annot
    return (x) => 3;
  },
  set soAreSetters(x) {
    const h = (x) => 3; // Missing annot
    return (x) => 3;
  } 
};
