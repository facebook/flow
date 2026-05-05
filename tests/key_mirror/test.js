declare function keyMirror<O>(o: O): $KeyMirror<O>;

var o = keyMirror({
  FOO: null,
  BAR: null,
});

o.FOO as 'FOO'; // ok
o.FOO as 'BAR'; // error, 'FOO' incompatible with 'BAR'


export type Props = Readonly<{
  a?: string,
  b?: string,
}>;

type KeyMirroredProps = Readonly<$KeyMirror<Props>>;

export type SpreadKeyMirroredProps = Readonly<{
  ...$Exact<KeyMirroredProps>,
}>;

// The following should be okay, since KeyMirroredProps should behave just like
// `Readonly<$ObjMapi<Props, <K>(K) => K>>` that preserves the optionality of the
// properties.
({b: 'b'} as SpreadKeyMirroredProps);

declare var badKeyMirror: $KeyMirror<1>;
badKeyMirror.f; // error
