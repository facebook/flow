declare function mergedFunction<T>(value: T): T;

declare namespace mergedFunction {
  type Identity<T> = T;
  export function member<T>(value: T): Identity<T>;
  export type NumberResult = Identity<number>;
}
