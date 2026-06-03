// Base class with a real static method, exported across module boundaries
// to exercise the static-side concretization in collect_inherited_members.

export class Counter {
  static reset(): void {}
}
