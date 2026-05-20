// Verify the declaration-site `this:`-parameter check is skipped in .ts files.
//
// Flow's check_methods previously enforced `self <: this_param` for every
// method declared with explicit `this:` annotation, requiring the enclosing
// interface/class to structurally satisfy the annotation. TypeScript only
// uses the annotation as a constraint on call-site `this`, not as a
// requirement on the declaring type. The patterns below all produce errors
// in .js but should be silent in .ts.

class HasExtra {
  extra: number = 0;
}

interface Sub extends HasExtra {
  label: string;
}

// Chart.js-style pattern: interface method with `this:` pointing at a related
// type whose super has properties not present on the enclosing interface.
interface Parent {
  onClick(this: Sub, label: string): void; // OK in .ts
}

// Same pattern with classes.
class ParentClass {
  handle(this: Sub, label: string): void {
    // Body sees `this` as `Sub`, so the `label` write is fine.
    this.label = label;
  }
}

// Static methods with explicit `this:` parameter — the same gate covers them.
class WithStatic {
  static run(this: Sub): void {} // OK in .ts
}

// `this:` annotation pointing at an entirely unrelated type — TS treats this
// as legal at the declaration site (the only effect is on call sites).
interface Unrelated {
  m(this: { foo: string }): void; // OK in .ts
}

// Method with no explicit `this:` parameter is unaffected (negative control).
interface NoThis {
  m(x: number): void;
}

// Sanity check: methods without `this:` still type-check normally.
declare const noThis: NoThis;
noThis.m(1); // OK
