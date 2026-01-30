class A {
  protected x: string;
  protected m(): void {}

  protected static x: string;
  protected static m(): void {}

  protected readonly y: string;
  protected static readonly z: string;
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
