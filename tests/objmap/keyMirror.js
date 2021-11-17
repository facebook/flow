declare function keyMirror<O>(o: O): $KeyMirror<O>;

var o = keyMirror({
  FOO: null,
  BAR: null,
});

(o.FOO : 'FOO'); // ok
(o.FOO : 'BAR'); // error, 'FOO' incompatible with 'BAR'


export type Props = $ReadOnly<{
  a?: string,
  b?: string,
}>;

type KeyMirroredProps = $ReadOnly<$KeyMirror<Props>>;

export type SpreadKeyMirroredProps = $ReadOnly<{
  ...$Exact<KeyMirroredProps>,
}>;

// The following should be okay, since KeyMirroredProps should behave just like
// `$ReadOnly<$ObjMapi<Props, <K>(K) => K>>` that preserves the optionality of the
// properties.
({b: 'b'}: SpreadKeyMirroredProps);

declare var badKeyMirror: $KeyMirror<1>;
badKeyMirror.f; // error
