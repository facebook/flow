declare function useCallback<T extends (...args: $ReadOnlyArray<empty>) => unknown>(
  callback: T,
  inputs: ?$ReadOnlyArray<unknown>
): T;

type ReturnValueType<T extends InputValueType> =
  T extends InputValueArrayType
    ? ReturnValueArrayType<T>
    : ReturnValueObjectType<T>;

type InputValueType = $ReadOnly<{[string]: InputValueType}>;
type InputValueArrayType = $ReadOnlyArray<InputValueType>;

type ReturnValueObjectType<T extends InputValueType> = $ReadOnly<{
  [key in keyof T]: ReturnValueType<T[key]>,
}>;

type ReturnValueArrayType<T extends InputValueArrayType> = $ReadOnlyArray<
  ReturnValueType<T[0]>,
>;

const foo: (
  id: string,
  type: InputValueType,
  key: string,
) => ReturnValueType<InputValueType> = useCallback(
  (
    id: string,
    type: InputValueType,
    key: string,
  ): ReturnValueType<InputValueType> => {
    return {
      [key]: foo(id, type[key], key),
    };
  },
  [],
);
