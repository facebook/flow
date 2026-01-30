class C {
  private x: string; // ERROR
  protected y: number; // ERROR
  public z: boolean; // ERROR

  private m1(): void {} // ERROR
  protected m2(): void {} // ERROR
  public m3(): void {} // ERROR

  private static a: string; // ERROR
  protected static b: number; // ERROR
  public static c: boolean; // ERROR

  // Combinations with readonly
  private readonly d: string; // ERROR
  protected readonly e: number; // ERROR
  public readonly f: boolean; // ERROR

  private static readonly g: string; // ERROR
  protected static readonly h: number; // ERROR
  public static readonly i: boolean; // ERROR
}

// Should still work when used as property names
class Valid {
  private: string;
  protected: number;
  public: boolean;
  static private: string;
}
