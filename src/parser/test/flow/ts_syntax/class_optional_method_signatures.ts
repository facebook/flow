class C {
  m?(): void;
  static s?(): string;
  generic?<T>(x: T): T;
  ["computed"]?(x: number): number;
  field?: string;
  unannotated?();
  after(): void {}
}
