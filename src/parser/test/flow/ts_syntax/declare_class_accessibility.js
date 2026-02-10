declare class C {
  private a;
  private b: number;
  private readonly c;
  private static d: string;
  protected e: number;
  public f: string;
  public readonly g: boolean;
  private h(): void;
  protected i(): number;
  public j(): string;
  protected static k: number;
  public static l: boolean;
  private static readonly m: string;
}

// accessibility + abstract
declare class D {
  protected abstract n: number;
  public abstract o: string;
  protected abstract p(): void;
  public abstract q(): number;
}

// accessibility modifiers used as property names
declare class E {
  private: string;
  protected: number;
  public: boolean;
}
