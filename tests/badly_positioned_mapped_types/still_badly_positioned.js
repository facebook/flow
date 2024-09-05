export type PickKeysFromObject<Spec, KeyMap> = {[K in keyof KeyMap]: Spec[K]};

type MySpec = {|
  x: number,
|};

(function() {
  const keys = {x: null, q: null};
  type A = PickKeysFromObject<MySpec, typeof keys>; // `q` is missing in `MySpec`
  declare var _a: {|x: number|};
  const _b: A = _a; // `q` is missing in object type
})();
