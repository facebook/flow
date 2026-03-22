class A {
  protected x: string;
  protected m(): void {}

  protected static x: string;
  protected static m(): void {}

  protected readonly y: string;
  protected static readonly z: string;
}

// accessibility with computed keys
class ComputedKeys {
  protected [sym]: number;
  protected [sym](): void {}
}

class NoIssueField {
  protected: string;
}
class NoIssueMethod {
  protected(): string {};
}
class NoIssueStaticField {
  static protected: string;
}
class NoIssueStaticMethod {
  static protected(): string {};
}
