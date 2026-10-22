// Enum members can use ordinary names, reserved words, and type keywords.
export declare enum E {
  literal, number, string, boolean, any, bigint, symbol, null, void,
  undefined, never, unknown, keyof, typeof, mixed, empty, bool,
  true, false, class, default,
  asserts, implies, is,
} // OK
export interface BaseElement<T extends E> { type: T } // OK

// Keyword members can appear in generic type arguments.
export type NumberElement = BaseElement<E.number>; // OK

// Qualified type names accept reserved words and contextual type keywords.
export type KeywordMembers =
  E.literal | E.number | E.string | E.boolean | E.any | E.bigint |
  E.symbol | E.null | E.void | E.undefined | E.never | E.unknown |
  E.keyof | E.typeof | E.mixed | E.empty | E.bool | E.true | E.false |
  E.class | E.default | E.asserts | E.implies | E.is; // OK

// The same identifier names are accepted after an import type qualification.
export type ImportedKeywordMembers =
  import('./qualified_type_keyword_members').E.number |
  import('./qualified_type_keyword_members').E.string |
  import('./qualified_type_keyword_members').E.boolean |
  import('./qualified_type_keyword_members').E.any |
  import('./qualified_type_keyword_members').E.bigint |
  import('./qualified_type_keyword_members').E.symbol |
  import('./qualified_type_keyword_members').E.null |
  import('./qualified_type_keyword_members').E.void |
  import('./qualified_type_keyword_members').E.undefined |
  import('./qualified_type_keyword_members').E.never |
  import('./qualified_type_keyword_members').E.unknown |
  import('./qualified_type_keyword_members').E.keyof |
  import('./qualified_type_keyword_members').E.typeof |
  import('./qualified_type_keyword_members').E.mixed |
  import('./qualified_type_keyword_members').E.empty |
  import('./qualified_type_keyword_members').E.asserts |
  import('./qualified_type_keyword_members').E.implies |
  import('./qualified_type_keyword_members').E.is; // OK

// Typeof qualifications also accept keyword members.
export type TypeofKeywordMember = typeof E.number; // OK
