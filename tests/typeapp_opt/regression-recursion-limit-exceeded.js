declare function useCallback<T: (...args: $ReadOnlyArray<empty>) => mixed>(
  callback: T,
  inputs: ?$ReadOnlyArray<mixed>
): T;

type ReturnValueType<T: InputValueType> =
  T extends InputValueArrayType
    ? ReturnValueArrayType<T>
    : ReturnValueObjectType<T>;

type InputValueType = $ReadOnly<{[string]: InputValueType}>;
type InputValueArrayType = $ReadOnlyArray<InputValueType>;

type ReturnValueObjectType<T: InputValueType> = $ReadOnly<{
  [key in keyof T]: ReturnValueType<T[key]>,
}>;

type ReturnValueArrayType<T: InputValueArrayType> = $ReadOnlyArray<
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
