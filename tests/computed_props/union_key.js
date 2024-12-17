type Params = $ReadOnly<{
  'a': boolean,
  'b': boolean,
}>;

declare var params: Params;

const test1 = <T: $Keys<typeof params>>(
  key: T,
): Params => {
  return {...params, [key]: true}; // ok: key set is normalized to StrT. error: indexed incompatible with Params
};

const test2 = <T: $Keys<Params>>(
  key: T,
): Params => {
  return {...params, [key]: true}; // ok: key set is normalized to StrT. error: indexed incompatible with Params
};

const test3 = <T: 'a' | 'b'>(
  key: T,
): Params => {
  return {...params, [key]: true}; // ok: key set is normalized to StrT. error: indexed incompatible with Params
};
