function test1() {
  const foo: ?string = 'foo';
  const baz: string = match([foo]) {
      [null | undefined] => 'empty',
      [const foo] => (() => {
        foo as empty; // error for sanity check
        return foo; // ok
      })(),
  }
}

function test2() {
  type X = {x: ?string}
  declare const a: X;
  declare const b: X;

  function test(a: X, b: X): string {
    return match ([a.x ?? null, b.x ?? null]) {
      [null, null] => 'nulls',
      [_, null] => 'b is null',
      [null, _] => 'a is null',
      // v0 and v1 filtered to be string
      [const v0, const v1] if (v0 === v1) => `${v0} === ${v1}`,
      const entireTuple => (() => {
        // We cannot filter out the entire tuple type. That will be unsound
        entireTuple as $ReadOnly<[string, string]>; // error
        return ''
      })(),
    }
  }
}

function test3() {
  opaque type MediaPayload = mixed;

  type Message = {
    kind: 'Video' | 'Image',
    mediaPayload: MediaPayload,
  } | {
    kind: 'Text',
    message: string,
  }

  function foo(t: Message): ?'Text' {
    return match (t) {
      {kind: 'Video', ...} as k => null,
      {kind: 'Image', ...} as k => null,
      {...} as k => k.kind, // ok
    }
  }

  function bar(t: Message): ?'Text' {
    switch (t.kind) {
      case 'Video': return null;
      case 'Image': return null;
      default: return t.kind; // ok
    }
  }
}
