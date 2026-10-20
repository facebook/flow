export class OptionalMethods {
  m?(x: number): string; // ERROR
  static s?(): number; // ERROR
  generic?<T>(x: T): T; // ERROR
  ["computed"]?(x: number): number; // ERROR
}

export class RequiredMethods {
  m(): void; // ERROR
}
