/**
 * @flow
 */

enum E {
  Foo,
  /** @deprecated */
  Bar,
}

E.
//^
