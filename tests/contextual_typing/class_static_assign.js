// Regression test: class declarations with static property assignments
// outside the class body must not produce spurious errors when there is
// no matching declare namespace. The assignments should be contextually
// typed from the class's declared static property types.

class Foo {
  static scanners: Map<string, number>;
  static items: Array<string>;
}
Foo.scanners = new Map();
Foo.items = [];

class Bar {
  static subscriptions: Map<string, number>;
}
Bar.subscriptions = new Map();

export class ExportedFoo {
  static data: Map<string, number>;
}
ExportedFoo.data = new Map();

export default class DefaultFoo {
  static data: Map<string, number>;
}
DefaultFoo.data = new Map();
