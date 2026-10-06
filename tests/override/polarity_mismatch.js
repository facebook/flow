// @flow

declare class A {
  p: string;
}

class Named extends A {
  readonly p: string = ''; // ERROR: read-only in `Named`
}

const Anonymous = class extends A {
  readonly p: string = ''; // ERROR: read-only in `<<anonymous class>>`
};

type Inline = interface extends A { readonly p: string }; // ERROR: read-only in the inline interface's type

class Generic<T> {
  p: T;
}

class FromGeneric<T> extends Generic<T> {
  readonly p: T; // ERROR: writable in `Generic<T>`
}

class OwnProto {
  readonly q: string = ''; // ERROR: read-only in `OwnProto` but write-only in `OwnProto`
  set q(x: string) {} // ERROR: duplicate member
}
