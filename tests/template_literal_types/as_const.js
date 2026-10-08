declare const s: string;
declare const n: number;

// Suffix only
{
  const x = `${s}dp` as const;
  x as `${string}dp`; // OK
  x as string; // OK
  x as `${string}px`; // ERROR
  x as `${string}`; // OK - wider template
}

// Prefix only
{
  const x = `data-${s}` as const;
  x as `data-${string}`; // OK
  x as string; // OK
  x as `info-${string}`; // ERROR
}

// Both prefix and suffix
{
  const x = `pre-${s}-suf` as const;
  x as `pre-${string}-suf`; // OK
  x as string; // OK
  x as `pre-${string}-other`; // ERROR
  x as `other-${string}-suf`; // ERROR
}

// Multiple placeholders
{
  const x = `a${s}b${n}c` as const;
  x as `a${string}b${number}c`; // OK
  x as string; // OK
  x as `a${string}b${string}c`; // OK - number stringifies to string
  x as `a${number}b${number}c`; // ERROR - string is not number
}

// No static text
{
  const x = `${s}` as const;
  x as string; // OK
  x as `${string}`; // OK
}

// Literal substitution folds to string literal
{
  const lit = "world";
  const x = `hello ${lit}` as const;
  x as "hello world"; // OK
  x as string; // OK
  x as "hello mars"; // ERROR
}

// Inline literal substitution folds
{
  const x = `n${123}` as const;
  x as "n123"; // OK
  x as string; // OK
  x as "n124"; // ERROR
}

// Union of literals distributes
{
  declare const u: "a" | "b";
  u as "a" | "b"; // OK
  u as string; // OK
  const x = `x${u}` as const;
  x as "xa" | "xb"; // OK
  x as string; // OK
  x as "xa"; // ERROR - missing "xb"
}

// Union of literals distributes (inferred union)
{
  declare const cond: boolean;
  const u = cond ? "a" : "b";
  u as "a" | "b"; // OK
  u as string; // OK
  const x = `x${u}` as const;
  x as "xa" | "xb"; // OK
  x as string; // OK
}

// Single literal via declare const
{
  declare const a: "a";
  const x = `x${a}` as const;
  x as "xa"; // OK
  x as string; // OK
}

// No substitution - plain string literal (already worked)
{
  const x = `hello` as const;
  x as "hello"; // OK
  x as string; // OK
  x as "bye"; // ERROR
}

// Without as const - const infers precisely, let widens
{
  const y = `${s}dp`;
  y as string; // OK
  y as `${string}dp`; // OK - const preserves template
  y as `${string}px`; // ERROR
}
{
  let z = `${s}dp`;
  z as string; // OK
  z as `${string}dp`; // ERROR - let widens to string
}

// Annotated init without as const - contextual precision
{
  const x: `pre-${string}-suf` = `pre-${s}-suf`; // OK
  const bad: `pre-${string}-other` = `pre-${s}-suf`; // ERROR
  let w: `pre-${string}-suf` = `pre-${s}-suf`; // OK - hint applies to let too
}

// Const literal substitution folds without as const
{
  const lit = "world";
  const x = `hello ${lit}`;
  x as "hello world"; // OK
  x as "hello mars"; // ERROR
}
{
  const x = `n${123}`;
  x as "n123"; // OK
  x as "n124"; // ERROR
}
{
  declare const u: "a" | "b";
  const x = `x${u}`;
  x as "xa" | "xb"; // OK
  x as "xa"; // ERROR
}

// Exported as const templates (exercises type_sig_merge)
export const exported_suffix = `${s}dp` as const;
export const exported_folded = `hello ${"world"}` as const;
// Exported plain const template (exercises const_decl merge)
export const exported_const_suffix = `${s}dp`;
export const exported_const_folded = `hello ${"world"}`;
