// Read-write property whose type narrows across an extends/implements/
// instantiation boundary. TS accepts these as covariant (assignable when
// the lower's type is a subtype of the upper's); Flow matches that in
// .ts instead of requiring invariant equality.

// Interface extends with a narrower property type (modeled on @babel/types).
interface BaseComment {
  type: "CommentBlock" | "CommentLine";
}
interface CommentBlock extends BaseComment {
  type: "CommentBlock"; // OK in .ts
}

// Class implements with a narrower property type.
interface Shape {
  kind: string;
}
declare class Circle implements Shape {
  kind: "circle"; // OK in .ts
}

// Generic instantiation that narrows a substituted property type.
interface Holder<T> {
  value: T;
}
interface StringHolder extends Holder<string> {
  value: "hello"; // OK in .ts
}

// The gate replaces invariance with covariance, not bivariance --
// genuinely incompatible types must still error.
interface Parent {
  tag: "a" | "b";
}
interface Child extends Parent {
  tag: "c"; // ERROR: "c" is not assignable to "a" | "b"
}
