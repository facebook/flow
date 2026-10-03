declare export function assertString(value: unknown): asserts value is string; // error: unsupported syntax

declare export function assertBare(value: unknown): asserts value; // error: unsupported syntax

declare export const annotated: (value: unknown) => asserts value is number; // error: unsupported syntax

export type ExportedAssertion = (value: unknown) => asserts value is boolean; // error: unsupported syntax
