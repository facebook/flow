/* @flow */

declare var x: string;

type O = {
  readwrite p: number, // access modifiers not yet supported
}

var o = {
  readwrite [x]: 0, // access modifiers not yet supported
  readwrite p: 0,   // access modifiers not yet supported
};

class C {
  readwrite p;      // access modifiers not yet supported
  readwrite m() {}; // access modifiers not yet supported
}

interface I {
  readwrite p: number; // access modifiers not yet supported
  readwrite m(): void; // access modifiers not yet supported
}

declare class D {
  readwrite p: number; // access modifiers not yet supported
  readwrite m(): void; // access modifiers not yet supported
}
