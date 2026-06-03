// Interface members do NOT count as override targets. A class that only
// `implements` an interface (no `extends`) has no super chain, so any
// `override` member triggers OverrideWithoutExtends.

interface I {
  greet(): string;
}

class Greeter implements I {
  override greet(): string { // ERROR: class has no `extends` clause
    return "hi";
  }
}
