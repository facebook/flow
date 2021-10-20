// @flow

declare var obj: { f: string, g: number };

declare var t: $ObjMapConst<typeof obj, boolean>;

declare export var t1: typeof t.f;
(t1: string); // error
(t1: boolean);


export type Props = $ReadOnly<{
  a?: string,
  b?: string,
}>;

export type OptionalProps = $ReadOnly<{
  ...$Exact<$ReadOnly<$ObjMapConst<Props, boolean>>>,
}>;

// The following should be okay, since OptionalProps should behave just like
// `$ReadOnly<$ObjMap<Props, () => boolean>>`, that preserves the optionality of the
// properties.
({b: true}: OptionalProps);
