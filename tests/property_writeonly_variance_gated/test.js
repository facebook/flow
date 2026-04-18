// Without experimental.allow_variance_keywords, writeonly errors with a
// gated-syntax message — but parses OK.
type T = { writeonly p: number };
class C {
  writeonly prop: number;
  constructor(p: number) {
    this.prop = p;
  }
}

// `writeonly` as identifier is unaffected.
const writeonly = 1;
type R = { writeonly: string };
