type Params = $ReadOnly<{
  'a': boolean,
  'b': boolean,
}>;

declare var params: Params;

const test1 = <T: $Keys<typeof params>>(
  key: T,
): Params => {
  return {...params, [key]: true}; // error key set is invalid-computed-prop
};

const test2 = <T: $Keys<Params>>(
  key: T,
): Params => {
  return {...params, [key]: true}; // error key set is invalid-computed-prop
};

const test3 = <T: 'a' | 'b'>(
  key: T,
): Params => {
  return {...params, [key]: true}; // error union is invalid-computed-prop
};
