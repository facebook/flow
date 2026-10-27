class C extends Base {
  private m(x: number): void;
  private m(x: string): void;
  private m(x: number | string) {}

  protected n(x: number): void;
  protected n(x: string): void;
  protected n(x: number | string) {}

  public p(x: number): void;
  public p(x: string): void;
  public p(x: number | string) {}

  protected override optional?<T>(x: T): T;
  public static [Symbol.iterator](): Iterator<number>;
  unmodified(): void;
}
