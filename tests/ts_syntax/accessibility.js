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

// Declare class accessibility modifiers
declare class D {
  private a; // ERROR
  private b: number; // ERROR
  private readonly c; // ERROR
  private static d: string; // ERROR
  protected e: number; // ERROR
  public f: string; // ERROR
  public readonly g: boolean; // ERROR
  private h(): void; // ERROR
  protected i(): number; // ERROR
  public j(): string; // ERROR
  protected static k: number; // ERROR
  public static l: boolean; // ERROR
  private static readonly m: string; // ERROR
}

// Declare class accessibility + abstract
declare class F {
  protected abstract n: number; // ERROR
  public abstract o: string; // ERROR
  protected abstract p(): void; // ERROR
  public abstract q(): number; // ERROR
}

// accessibility modifiers as property names in declare class
declare class E {
  private: string;
  protected: number;
  public: boolean;
}
