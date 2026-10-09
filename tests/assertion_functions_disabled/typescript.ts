export declare function assertString(value: unknown): asserts value is string;
declare function assertTruthy(value: unknown): asserts value;

function typed(value: unknown): string {
  assertString(value);
  const invalid: number = value; // ERROR
  return value;
}

function bare(value: string | null | undefined): string {
  assertTruthy(value);
  return value;
}
