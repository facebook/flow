class A {
  public x: string;
  public m(): void {}

  public static x: string;
  public static m(): void {}

  public readonly y: string;
  public static readonly z: string;
}

class NoIssueField {
  public: string;
}
class NoIssueMethod {
  public(): string {};
}
class NoIssueStaticField {
  static public: string;
}
class NoIssueStaticMethod {
  static public(): string {};
}
