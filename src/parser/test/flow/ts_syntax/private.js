class A {
  private x: string;
  private m(): void {}

  private static x: string;
  private static m(): void {}

  private readonly y: string;
  private static readonly z: string;
}

// accessibility with computed keys
class ComputedKeys {
  private [sym]: string;
  private [sym](): void {}
}

class NoIssueField {
  private: string;
}
class NoIssueMethod {
  private(): string {};
}
class NoIssueStaticField {
  static private: string;
}
class NoIssueStaticMethod {
  static private(): string {};
}
