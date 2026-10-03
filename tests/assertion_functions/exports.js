declare export function assertNumber(value: unknown): asserts value is number;
declare function assertString(value: unknown): asserts value is string;

declare const annotated: (value: unknown) => asserts value is number;
export {annotated};

export const inferred = (value: unknown): asserts value is number => assertNumber(value);
export const inferredAlias = assertNumber;
export const inferredObject = {assertNumber};

declare const assertions: {
  nested: {
    assertNumber(value: unknown): asserts value is number,
  },
};
export {assertions};

export default assertString;

export class ExportedAssertions {
  static assertNumber(value: unknown): asserts value is number {
    assertNumber(value);
  }
}

export type ImportedAssertion = (value: unknown) => asserts value is number;

export type ImportedAssertionFor<T> = (value: unknown) => asserts value is T;

export type RenamedImportedAssertion = (input: unknown) => asserts input is number;

export type ImportedBareAssertion = (value: unknown) => asserts value;

declare export const agreedUnionAssertion: ImportedAssertion | RenamedImportedAssertion;

declare export const disagreedUnionAssertion: ImportedAssertion | ImportedBareAssertion;

declare export const genericAppAssertion: ImportedAssertionFor<symbol>;

export type ImportedAssertionShape = {
  number: number,
  string: string,
};

declare export const importedMappedAssertions: {
  [K in keyof ImportedAssertionShape]:
    (value: unknown) => asserts value is ImportedAssertionShape[K],
};
