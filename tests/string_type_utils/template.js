declare var n: number;

// Suffix only
{
  const x: StringSuffix<'dp'> = `${n}dp`; // OK
  const y: StringSuffix<'%'> = `${n}%`; // OK
}

// Prefix only
{
  const x: StringPrefix<'data-'> = `data-${n}`; // OK
  const y: StringPrefix<'$'> = `$${n}`; // OK
}

// Both prefix and suffix
{
  const x: StringPrefix<'pre-'> = `pre-${n}-suf`; // OK
  const y: StringSuffix<'-suf'> = `pre-${n}-suf`; // OK
  const z: `pre-${string}-suf` = `pre-${n}-suf`; // OK
}

// Still assignable to string
{
  const x: string = `${n}dp`; // OK
  const y: string = `abc${n}`; // OK
  const z: string = `abc${n}dp`; // OK
  const w: string = `${n}`; // OK
}

// Wrong suffix should error
{
  const x: StringSuffix<'px'> = `${n}dp`; // ERROR
}

// Wrong prefix should error
{
  const x: StringPrefix<'foo'> = `bar${n}`; // ERROR
}

// No prefix/suffix (empty quasis) should error
{
  const x: StringSuffix<'dp'> = `${n}`; // ERROR
  const y: StringPrefix<'abc'> = `${n}`; // ERROR
}

// Multiple expressions
{
  const x: StringPrefix<'pre-'> = `pre-${n}-${n}`; // OK
  const y: StringSuffix<'-suf'> = `${n}-${n}-suf`; // OK
  const z: StringPrefix<'a'> = `a${n}b${n}c`; // OK
  const w: StringSuffix<'c'> = `a${n}b${n}c`; // OK
}

// No annotation — const keeps template precision, let widens to string
{
  const y = `${n}dp`;
  (y as string); // OK
  (y as `${string}dp`); // OK - const preserves template
  (y as `${string}px`); // ERROR
}

{
  let z = `${n}dp`;
  (z as string); // OK
  (z as `${string}dp`); // ERROR - let widens to string
}

// Reads generalize like string literals: `let` widens, `const` keeps
{
  const tmpl = `${n}dp`;
  let w = tmpl;
  w as string; // OK
  w as `${string}dp`; // ERROR - let generalizes on read
  const k = tmpl;
  k as `${string}dp`; // OK - const keeps precision
}

// Function parameter with StringSuffix hint
{
  declare function f(x: StringSuffix<'dp'>): void;
  f(`${n}dp`); // OK
}

// Implicit instantiation — unhinted generic generalizes to string,
// like string literals (`useState(42)` gives `number`, not `42`)
{
  declare function identity<T extends string>(value: T): T;
  const value = identity(`${n}dp`);
  value as string; // OK
  value as StringSuffix<'dp'>; // ERROR - T generalizes to string

  const contextual: StringSuffix<'dp'> = identity(`${n}dp`); // OK - hint keeps precision
}

// Implicit instantiation — wrapper generic generalizes too
{
  declare function wrap<T extends string>(value: T): {value: T};
  const wrapped = wrap(`${n}dp`);
  wrapped.value as string; // OK
  wrapped.value as StringSuffix<'dp'>; // ERROR - T generalizes to string
}

// Implicit instantiation — unhinted generic loses prefix and suffix
{
  declare function identity<T extends string>(value: T): T;
  const value = identity(`pre-${n}-suf`);
  value as string; // OK
  value as `pre-${string}-suf`; // ERROR - T generalizes to string
  value as StringPrefix<'pre-'>; // ERROR
  value as StringSuffix<'-suf'>; // ERROR
  value as `other-${string}-suf`; // ERROR
  value as `pre-${string}-other`; // ERROR
}

// Implicit instantiation — nested call keeps precision that is checked
// against an annotation, like literals do (cf. x4 in nested_generic_calls)
{
  declare var someStr: string;
  declare class CC<T> {}
  declare function ff2<T extends string>(literalValue: T): CC<T>;
  declare function gg2<T extends string>(literalValue: CC<T>): CC<CC<T>>;
  const tx: CC<CC<`${string}dp`>> = gg2(ff2(`${someStr}dp`)); // OK
}

// Parity reference: string literals behave the same through generics
{
  declare function id<T extends string>(x: T): T;
  const v = id('hello');
  v as string; // OK
  v as 'hello'; // ERROR - T generalizes to string
}
{
  declare class DD<T> {}
  declare function ff3<T extends string>(literalValue: T): DD<T>;
  declare function gg3<T extends string>(literalValue: DD<T>): DD<DD<T>>;
  const stx: DD<DD<'hello'>> = gg3(ff3('hello')); // OK
}

// Implicit instantiation — wrong suffix through generic errors
{
  declare function identity<T extends string>(value: T): T;
  const invalid: StringSuffix<'px'> = identity(`${n}dp`); // ERROR
}

// Implicit instantiation — no prefix/suffix stays string
{
  declare function identity<T extends string>(value: T): T;
  const value = identity(`${n}`);
  value as string; // OK
}

// Implicit instantiation — annotation-originated templates never generalize
{
  declare function identity<T extends string>(value: T): T;
  declare const t: `${string}dp`;
  const v = identity(t);
  v as `${string}dp`; // OK
  v as string; // OK
}

// Implicit instantiation — `as const` templates never generalize, like literals
{
  declare function identity<T extends string>(value: T): T;
  const v = identity(`${n}dp` as const);
  v as `${string}dp`; // OK
  v as `${string}px`; // ERROR
}

// Implicit instantiation — folded templates generalize like plain literals
{
  declare function identity<T extends string>(value: T): T;
  const lit = 'world';
  const v = identity(`hello ${lit}`);
  v as string; // OK
  v as 'hello world'; // ERROR - folded value generalizes like a literal
}

// `as const` templates stay precise on reads, like `as const` strings
{
  const c = `${n}dp` as const;
  let w = c;
  w as `${string}dp`; // OK - frozen
  w as string; // OK
}

// String concatenation with += and +
{
  let formatted = '';
  declare var x: string;
  formatted += `-${x}`; // OK
}

{
  declare var s: string;
  const result = s + `${n}dp`; // OK
}

// Template literal in object property must not leak hint constraints
{
  declare var color: string;
  function getDiffStyle(
    s: string | null | void,
  ): {outline: string, opacity?: number} | null {
    if (s === 'added') {
      return {outline: `3px solid ${color}`};
    } else if (s === 'removed') {
      return {outline: `3px solid ${color}`, opacity: 0.5};
    }
    return null;
  }
}

// Enclosing container hints preserve constrained template literal precision.
{
  const object: {width: StringSuffix<'dp'>} = {width: `${n}dp`}; // OK
  const array: Array<StringSuffix<'dp'>> = [`${n}dp`]; // OK
}

// Template literal in mutable object — no invariant check interference
{
  declare var clr: string;
  declare var dir: string;
  let style: {borderTop?: string, borderBottom?: string} = {};
  style =
    dir === 'before'
      ? {borderBottom: `2px solid ${clr}`}
      : {borderTop: `2px solid ${clr}`};
}

// Casts retain the same contextual behavior as annotations.
{
  `${n}dp` as StringSuffix<'dp'>; // OK
  `${n}px` as StringSuffix<'dp'>; // ERROR
}
