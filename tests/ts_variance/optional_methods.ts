export interface OptionalReturn<T> {
  return?(value?: T): T;
}

interface RequiredReturn<T> extends OptionalReturn<T> {
  return(value: T): T;
}

export interface Animal {
  name: string;
}

export interface Dog extends Animal {
  bark(): void;
}

export interface OptionalMethod {
  cb?(value: Animal): void;
}

export interface OptionalGenericMethod {
  cb?<T>(value: T, animal: Animal): T;
}

export interface OptionalFunctionField {
  cb?: (value: Animal) => void;
}

export declare class OptionalClassMethod {
  cb?(value: Animal): void;
  static cb?(value: Animal): void;
}

declare const key: 'cb';
export interface OptionalComputedMethod {
  [key]?(value: Animal): void;
}

export declare const narrow: {cb(value: Dog): void};
export declare const optionalNarrow: {cb?(value: Dog): void};
export declare const genericNarrow: {cb<T>(value: T, animal: Dog): T};

const method: OptionalMethod = narrow;
const optionalMethod: OptionalMethod = optionalNarrow;
const genericMethod: OptionalGenericMethod = genericNarrow;
const classMethod: OptionalClassMethod = narrow;
const computedMethod: OptionalComputedMethod = narrow;
const functionField: OptionalFunctionField = narrow; // ERROR

OptionalClassMethod.cb?.({name: 'animal'});
const callback: ((value: Animal) => void) | undefined = OptionalClassMethod.cb;

const literal: {cb?(value: Animal): void} = narrow;
const optionalLiteral: {cb?(value: Animal): void} = optionalNarrow;

declare const unrelated: {cb(value: number): void};
const unrelatedMethod: OptionalMethod = unrelated; // ERROR

declare const wrongReturn: {cb(value: Dog): number};
const returnMismatch: {cb?(value: Animal): string} = wrongReturn; // ERROR

declare const extraArgument: {cb(value: Dog, extra: number): void};
const arityMismatch: OptionalMethod = extraArgument; // ERROR

const missingMethod: {cb(value: Animal): void} = optionalNarrow; // ERROR

declare const optionalGenericNarrow: {cb?<T>(value: T, animal: Dog): T};
const optionalGenericMethod: OptionalGenericMethod = optionalGenericNarrow;
