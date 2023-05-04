type Params = $ReadOnly<{
  'a': boolean,
  'b': boolean,
}>;

declare var params: Params;

const test0 = (): Params => {
  return {...params, [true]: true}; // error true is invalid-computed-prop
};

const test1 = <T: boolean>(
  key: T,
): Params => {
  return {...params, [key]: true}; // error boolean is invalid-computed-prop
};

const test2 = <T: true>(
  key: T,
): Params => {
  return {...params, [key]: true}; // error true is invalid-computed-prop
};
